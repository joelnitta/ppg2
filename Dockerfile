FROM rocker/shiny:4.2.1

# Install system deps ----

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    git \
    build-essential \
    gawk

# Install R packages with renv ----

# Create directory for renv project library
RUN mkdir /renv

# Modify Rprofile.site so renv uses /renv for project library
RUN echo 'Sys.setenv(RENV_PATHS_LIBRARY = "/renv")' >> /usr/local/lib/R/etc/Rprofile.site

# Initialize a 'dummy' project and restore the renv library.
# Since the library path is specified as above, the library will be restored to /renv
RUN mkdir /tmp/project

COPY ./renv.lock /tmp/project

WORKDIR /tmp/project

# Turn off cache
RUN Rscript -e 'install.packages("renv"); renv::consent(provided = TRUE); renv::settings$use.cache(FALSE); renv::init(bare = TRUE); renv::restore()'

# Install other software ----

### taxon-tools ###
ENV APP_NAME=taxon-tools
ENV TAXONTOOLS_VERSION=8f8b5e2611b6fdef1998b7878e93e60a9bc7c130
RUN git clone https://github.com/camwebb/$APP_NAME.git && \
  cd $APP_NAME && \
  git checkout $TAXONTOOLS_VERSION && \
  make check && \
  make install

### github cli ###
RUN type -p curl >/dev/null || sudo apt install curl -y
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
  && sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
  && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
  && sudo apt update \
  && sudo apt install gh -y

# copy the app directory into the image
COPY ./app/ /srv/shiny-server/

# copy renv startup scripts to app
COPY ./renv/ /srv/shiny-server/renv
COPY .Rprofile /srv/shiny-server/

# run app
CMD ["/usr/bin/shiny-server"]