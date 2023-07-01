FROM openanalytics/r-ver:4.1.3

LABEL maintainer="Boris Delange <linkr-app@pm.me>"

# System libraries of general use
RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxml2 \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required for the app
RUN R -e "install.packages(c('clipr', 'curl', 'DBI', 'dplyr', 'DT', 'ggplot2', 'golem', 'glue', 'knitr', 'magrittr', 'pkgload', 'plotly', 'pryr', 'readr', 'remotes', 'rlang', 'rlist', 'rmarkdown', 'RPostgres', 'RSQLite', 'shiny', 'shiny.fluent', 'shiny.i18n', 'shinymanager', 'shiny.react', 'shiny.router', 'shinyAce', 'shinybusy', 'shinyjs', 'sortable', 'stringr', 'tidyr', 'XML', 'zip'), repos='https://cloud.r-project.org/')"

# Install LinkR from GitHub
RUN R -e "remotes::install_github('BorisDelange/LinkR')"

# Copy the app to the image
RUN mkdir /root/LinkR
COPY LinkR /root/LinkR

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838
CMD ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');linkr::linkr(language = 'fr', app_folder = '/root')"]