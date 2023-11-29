FROM rocker/tidyverse:latest

LABEL maintainer="Boris Delange <linkr-app@pm.me>"

# System libraries of general use
RUN apt-get update
    
# Install remotes
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"

# Install shiny version 1.7.4.1 specifically (last version working with LinkR)
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.7.4.1.tar.gz', repos=NULL, type='source')"

# Install LinkR from GitHub
RUN R -e "remotes::install_gitlab('interhop/linkr/linkr', host = 'framagit.org')"

# Copy the app to the image
RUN mkdir /root/LinkR
COPY LinkR /root/LinkR

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838
CMD ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');linkr::linkr(language = 'fr', app_folder = '/root')"]