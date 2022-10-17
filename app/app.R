library(shiny)
library(dwctaxon) # need for dct_validate

# Setup ----

# Load functions
if (file.exists(here::here("app/functions.R"))) {
  source(here::here("app/functions.R"))
} else if (file.exists(here::here("functions.R"))) {
  source(here::here("functions.R"))
} else {
  stop("Can't find functions.R")
}

# Load pteridocat with higher-level taxonomy
# (produced by data_prep.R)
if (file.exists(here::here("app/pteridocat_tax.RDS"))) {
  pteridocat_tax <- readRDS(here::here("app/pteridocat_tax.RDS"))
} else if (file.exists(here::here("pteridocat_tax.RDS"))) {
  pteridocat_tax <- readRDS(here::here("pteridocat_tax.RDS"))
} else {
  stop("Can't find pteridocat_tax.RDS")
}

# Set up taxonomy lists for filtering
genera <- sort(unique(pteridocat_tax$genus))
subfamilies <- sort(unique(pteridocat_tax$subfamily))
families <- sort(unique(pteridocat_tax$family))
suborders <- sort(unique(pteridocat_tax$suborder))
orders <- sort(unique(pteridocat_tax$order))
classes <- sort(unique(pteridocat_tax$class))

genus_select <- genera
subfamily_select <- subfamilies
family_select <- families
suborder_select <- suborders
order_select <- orders
class_select <- classes

genus_select <- NA
subfamily_select <- NA
family_select <- NA
suborder_select <- NA
order_select <- NA
class_select <- NA

# Minimize columns for download: user is only exposed to these,
# others can be inferred by backend
keep_cols <- c(
  "taxonID",
  "acceptedNameUsageID",
  "taxonomicStatus",
  "taxonRank",
  "scientificName",
  "taxonRemarks",
  "namePublishedIn",
  "references"
)

# official columns in pteridocat
keep_cols_detailed <- c(
  "taxonID",
  "parentNameUsageID",
  "acceptedNameUsageID",
  "taxonomicStatus",
  "taxonRank",
  "scientificName",
  "genericName",
  "infragenericEpithet",
  "specificEpithet",
  "infraspecificEpithet",
  "namePublishedIn",
  "nomenclaturalCode",
  "nomenclaturalStatus",
  "taxonRemarks",
  "references",
  "modified",
  "nameAccordingTo"
)

# subset pteridocat + higher level taxonomy to just pteridocat
pteridocat <- dplyr::select(pteridocat_tax, dplyr::all_of(keep_cols_detailed))

ui <- fluidPage(
  tabsetPanel(
    # First main tab is to select and download data
    tabPanel(
      "Download",
      sidebarLayout(
        sidebarPanel(
          selectInput("genus",
            label = "Genus", choices = genera,
            multiple = TRUE
          ),
          selectInput("subfamily",
            label = "Subfamily", choices = subfamilies,
            multiple = TRUE
          ),
          selectInput("family",
            label = "Family", choices = families,
            multiple = TRUE
          ),
          selectInput("suborder",
            label = "Suborder", choices = suborders,
            multiple = TRUE
          ),
          selectInput("order",
            label = "Order", choices = orders,
            multiple = TRUE
          ),
          selectInput("class",
            label = "Class", choices = classes,
            multiple = TRUE
          ),
          downloadButton("download", "Download .csv")
        ),
        mainPanel(
          dataTableOutput("preview")
        )
      )
    ),
    # Second main tab is to upload and verify data
    tabPanel(
      "Upload",
      sidebarLayout(
        sidebarPanel(
          # Upload button
          fileInput("upload", "Upload data", multiple = FALSE, accept = ".csv"),
          textInput("user", "User name"),
          textInput("email", "Email"),
          passwordInput("pat", "GitHub PAT", value = Sys.getenv("GITHUB_PAT")),
          textAreaInput("changes_summary", "Summary of changes", rows = 3),
          # Summary of data upload
          textOutput("data_new_summary"),
          # Push changes button
          actionButton("push_git", "Push to GitHub")
        ),
        mainPanel(
          dataTableOutput("preview_ul")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Server code for download tab
  data_dl <- reactive({
      pteridocat_tax |>
      # Apply taxon filter
      dplyr::filter(
        genus %in_f_na% input$genus |
        family %in_f_na% input$family |
        subfamily %in_f_na% input$subfamily |
        suborder %in_f_na% input$suborder |
        order %in_f_na% input$order |
        class %in_f_na% input$class
      ) |>
      # Keep only standard columns
      dplyr::select(dplyr::all_of(keep_cols))
  })
  output$preview <- renderDataTable(
    data_dl(),
    options = list(
      pageLength = 10
    )
  )
  output$download <- downloadHandler(
    filename = function() {
      return("ppg2_data.csv")
    },
    content = function(file) {
      dat <- dplyr::mutate(data_dl(), new = 0)
      write.csv(dat, file = file, row.names = FALSE)
    }
  )
  # Server code for upload tab
  data_ul <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$datapath)
    switch(
      ext,
      csv = readr::read_csv(
        input$upload$datapath,
        col_types = readr::cols(.default = readr::col_character())
      ),
      validate("Invalid file; please upload a .csv file")
    )
  })
  output$preview_ul <- renderDataTable(
    data_ul(),
    options = list(
      pageLength = 10
    )
  )
  # - validation and intergration of new data
  # data_new() is the new pteridocat db
  data_new <- reactive({
    req(data_ul())
    new_dat_raw <- verify_ul(
      pteridocat = pteridocat,
      data_ul = data_ul(),
      valid_col_names = c(keep_cols, "new")
    )
    make_new_data(new_dat_raw, pteridocat)
  })
  string <- reactive(
    paste0(
      "Rows uploaded: ",
      nrow(data_ul()),
      " Rows total: ",
      nrow(data_new()), " ", getwd())
  )
  output$data_new_summary <- renderText(string())
  # - push changes to repo
  observeEvent(input$push_git, {
    req(data_new())
    req(input$user)
    req(input$email)
    req(input$pat)
    req(input$changes_summary)
    push_pterido(
      pteridocat = data_new(),
      user = input$user,
      email = input$email,
      changes_summary = input$changes_summary,
      pat = input$pat
    )
  })
}

shinyApp(ui, server)