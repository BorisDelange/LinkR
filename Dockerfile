FROM rocker/tidyverse:latest

LABEL maintainer="Boris Delange <linkr-app@pm.me>"

# Instal python and python libs
RUN apt-get update && apt-get install -y python3 python3-dev python3-pip python3-venv
RUN pip install numpy pandas matplotlib seaborn scipy scikit-learn imblearn xgboost pyarrow

# Install pak
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org/')"

# Install specific versions of R libs
RUN R -e "pak::pkg_install(c(
  'bit64@4.0.5',
  'clipr@0.8.0',
  'curl@5.0.0',
  'DBI@1.1.1',
  'dplyr@1.0.7',
  'DT@0.19',
  'duckdb@0.9.2-1',
  'dygraphs@1.1.1.6',
  'fontawesome@0.5.0',
  'ggplot2@3.3.5',
  'git2r@0.30.1',
  'golem@0.3.1',
  'glue@1.4.2',
  'httr@1.4.2',
  'knitr@1.36',
  'leaflet@2.2.0',
  'magrittr@2.0.1',
  'plotly@4.10.0',
  'pryr@0.1.5',
  'readr@2.0.2',
  'reticulate@1.34.0',
  'rlang@0.4.12',
  'rlist@0.4.6.2',
  'RPostgres@1.4.1',
  'RSQLite@2.2.8',
  'shiny@1.6.0',
  'shiny.fluent@0.3.0',
  'shiny.i18n@0.2.0',
  'shinymanager@1.0.400',
  'shiny.react@0.2.3',
  'shiny.router@0.3.1',
  'shinyAce@0.4.1',
  'shinybusy@0.2.2',
  'shinyjs@2.0.0',
  'sparklyr@1.8.4',
  'stringr@1.5.0',
  'tidyr@1.1.4',
  'vroom@1.5.7',
  'XML@3.99-0.3',
  'xts@0.13.2',
  'zip@2.2.0'
))"

# Config python3 use in R with reticulate
RUN R -e "reticulate::use_python('usr/bin/python3')"

# Install LinkR
ARG CACHEBUST=1
RUN R -e "pak::pkg_install('framagit::interhop/linkr/linkr')"

COPY Rprofile.site /usr/lib/R/etc/

# Run LinkR
EXPOSE 3838
CMD ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');linkr::linkr(language = 'fr', app_folder = '/root')"]