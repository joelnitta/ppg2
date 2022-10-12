library(shiny)
library(taxastand)
library(dwctaxon)
library(tidyverse)
library(assertr)
library(rgnparser)
library(taxastand)

source(here::here("R/functions.R"))

# Load data ----

pteridocat <- read_csv(
  here::here("data/pteridocat.csv"),
  col_types = cols(.default = col_character()))

ppgi <- read_csv(
  here::here("data/ppgi_taxonomy_mod.csv"),
  col_types = cols(.default = col_character())) %>%
  select(-notes)

# Wrangle data ----

# Many old synonyms in pteridocat have no entries for that genus (old genera)
# instead, grab the higher level taxonomy **for the accepted name**
# Output is df with taxonID and higher level taxonomy
higher_tax_for_syns <-
  pteridocat %>%
  filter(!is.na(acceptedNameUsageID)) %>%
  select(taxonID_orig = taxonID, acceptedNameUsageID) %>%
  left_join(
    pteridocat,
    by = c(acceptedNameUsageID = "taxonID")) %>%
  assert(not_na, genericName) %>%
  select(taxonID = taxonID_orig, genericName) %>%
  left_join(ppgi, by = c(genericName = "genus")) %>%
  assert(is_uniq, taxonID) %>%
  assert(not_na, class)

# do the same for accepted names
higher_tax_for_acc <-
  pteridocat %>%
  filter(is.na(acceptedNameUsageID)) %>%
  select(taxonID, genericName) %>%
  left_join(ppgi, by = c(genericName = "genus")) %>%
  assert(is_uniq, taxonID) %>%
  assert(not_na, class)

# combine them
higher_tax <- bind_rows(
  higher_tax_for_syns,
  higher_tax_for_acc
) %>%
  # 'genus' is PPGII genus (of accepted name),
  # genericName is original genus
  rename(genus = genericName) %>%
  assert(is_uniq, taxonID) %>%
  assert(not_na, class)

# Join higher-level taxonomy to pteridocat
pteridocat_tax <-
  pteridocat %>%
  assert(not_na, genericName) %>%
  left_join(higher_tax, by = "taxonID") %>%
  assert(is_uniq, taxonID)

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

# Prepare data download ----

# Minimize columns: user is only exposed to these,
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
          fileInput("upload", "Upload data", multiple = FALSE, accept = ".csv"),
          textOutput("data_new_summary")
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
      pteridocat_tax %>%
      # Apply taxon filter
      filter(
        genus %in_f_na% input$genus |
        family %in_f_na% input$family |
        subfamily %in_f_na% input$subfamily |
        suborder %in_f_na% input$suborder |
        order %in_f_na% input$order |
        class %in_f_na% input$class
      ) %>%
      # Keep only standard columns
      select(all_of(keep_cols))
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
      dat <- mutate(data_dl(), new = 0)
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
  # - validation
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
      nrow(data_new()))
  )
  output$data_new_summary <- renderText(string())
}

shinyApp(ui, server)