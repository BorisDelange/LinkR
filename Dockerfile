FROM rocker/tidyverse:latest

LABEL maintainer="Boris Delange <linkr-app@pm.me>"

# System libraries of general use
RUN apt-get update

# Install additional packages
RUN R -e "install.packages(c('remotes', shiny', xts', 'dygraphs', 'duckdb', 'sparklyr'), repos='https://cloud.r-project.org/')"

# Install LinkR from GitHub
RUN R -e "remotes::install_gitlab('interhop/linkr/linkr', host = 'framagit.org')"

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838
CMD ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');linkr::linkr(language = 'fr', app_folder = '/root')"]