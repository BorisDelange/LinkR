FROM rocker/tidyverse:latest

LABEL maintainer="Boris Delange <linkr-app@pm.me>"

# Install Python and Python libs
RUN apt-get update && apt-get install -y python3 python3-dev python3-pip python3-venv
RUN apt-get update && apt-get install -y python3 python3-dev python3-pip python3-venv
RUN python3 -m venv /opt/venv
RUN /opt/venv/bin/pip install --upgrade pip
RUN /opt/venv/bin/pip install numpy pandas matplotlib seaborn scipy scikit-learn imblearn xgboost pyarrow

# Install other R packages
RUN R -e "install.packages(c(\
  'dygraphs',\
  'reticulate',\
  'sparklyr',\
  'xts'\
))"

# Config python3 use in R with reticulate
RUN R -e "reticulate::use_python('/opt/venv/bin/python', required = TRUE)"

# Install shiny.fluent v0.3.0
RUN R -e "remotes::install_github('Appsilon/shiny.fluent', ref = 'dd1c956')"

# Install LinkR
RUN R -e "remotes::install_gitlab('interhop/linkr/linkr', host = 'framagit.org')"

COPY Rprofile.site /usr/lib/R/etc/

# Run LinkR
EXPOSE 3838
CMD ["R", "-e", "linkr::linkr(language = 'fr', app_folder = '/root')"]