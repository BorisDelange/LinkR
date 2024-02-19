FROM rocker/tidyverse:latest

LABEL maintainer="Boris Delange <linkr-app@pm.me>"

RUN apt-get update && apt-get install -y python3 python3-dev  python3-pip python3-venv
RUN pip install numpy pandas matplotlib seaborn scipy scikit-learn imblearn xgboost

RUN R -e "install.packages(c('remotes', 'shiny', 'xts', 'dygraphs', 'duckdb', 'sparklyr', 'reticulate'), repos='https://cloud.r-project.org/')"
RUN R -e "reticulate::use_python('usr/bin/python3')"
RUN R -e "remotes::install_gitlab('interhop/linkr/linkr', host = 'framagit.org')"

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838
CMD ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');linkr::linkr(language = 'fr', app_folder = '/root')"]
