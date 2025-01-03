FROM rocker/tidyverse:latest

LABEL maintainer="Boris Delange <linkr-app@pm.me>"

# Install Python and Python libs
RUN apt-get update && apt-get install -y python3 python3-dev python3-pip python3-venv
RUN apt-get update && apt-get install -y python3 python3-dev python3-pip python3-venv
RUN python3 -m venv /opt/venv
RUN /opt/venv/bin/pip install --upgrade pip
RUN /opt/venv/bin/pip install numpy pandas matplotlib seaborn scipy scikit-learn imblearn xgboost pyarrow

# The `options` function sets the host and port configurations for the Shiny application:
# - `shiny.port = 3838`: Specifies the port on which the Shiny application will run inside the container.
#   This port must match the one exposed in the Dockerfile and mapped on the host during deployment.
# - `shiny.host = '0.0.0.0'`: Allows the application to listen on all network interfaces, 
#   making it accessible externally. This is necessary for Docker to expose the application.
RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site

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

# Expose port 3838 for the Shiny application
EXPOSE 3838

# Run LinkR
CMD ["R", "-e", "linkr::linkr(language = 'fr', app_folder = '/root', debug = TRUE)"]