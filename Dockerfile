FROM rocker/r-ver:4.0.5

MAINTAINER Emmanuel Blondel "eblondel.pro@gmail.com"

# system libraries of general use

RUN apt-get update && apt-get install -y \
    sudo \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git 

# install dependencies of the app

#R CRAN packages
RUN R -e "install.packages(c('devtools'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('XML'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shiny'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinyjs'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinydashboard'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('jsonlite'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('DT'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('tibble'), repos='https://cran.r-project.org/')"

#R GitHub packages (with release in CRAN, but not re-published yet)
RUN R -e "devtools::install_github('eblondel/ows4R')"
RUN R -e "devtools::install_github('eblondel/geonapi')"
RUN R -e "devtools::install_github('eblondel/ocs4R')"

#R GitHub packages (not yet released in CRAN)
RUN R -e "devtools::install_github('eblondel/d4storagehub4R')"
RUN R -e "devtools::install_github('eblondel/geoflow')"

RUN git -C /root/ clone https://github.com/eblondel/geoflow-shiny.git && echo "OK!"
RUN ln -s /root/geoflow-shiny /srv/geoflow-shiny

#etc dirs (for config)
RUN mkdir -p /etc/geoflow-shiny/

#geoflow data dir
RUN mkdir -p /srv/geoflow-data/
ENV GEOFLOW_DATA_DIR="/srv/geoflow-data"
 
EXPOSE 3838

VOLUME ["srv/geoflow-data","/etc/geoflow-shiny"]
RUN apt-get install -y curl
CMD ["R", "-e shiny::runApp('/srv/geoflow-shiny',port=3838,host='0.0.0.0')"]
#CMD ["R", "-e shiny::runApp('/srv/geoflow-shiny')"]
