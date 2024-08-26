FROM rocker/tidyverse:latest

LABEL maintainer="Boris Delange <linkr-app@pm.me>"

# Instal python and python libs
RUN apt-get update && apt-get install -y python3 python3-dev python3-pip python3-venv
RUN pip install numpy pandas matplotlib seaborn scipy scikit-learn imblearn xgboost pyarrow

# Install other R packages
RUN R -e "install.packages(c(\
  'duckdb',\
  'dygraphs',\
  'markdown',\
  'reticulate'\
  'sparklyr',\
  'xts'\
))"

# Config python3 use in R with reticulate
RUN R -e "reticulate::use_python('usr/bin/python3')"

# Install shiny.fluent v0.3.0
RUN R -e "remotes::install_github('Appsilon/shiny.fluent', ref = 'dd1c956')"

# Install LinkR
ARG CACHEBUST=1
RUN R -e "remotes::install_gitlab('interhop/linkr/linkr', host = 'framagit.org')"

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838