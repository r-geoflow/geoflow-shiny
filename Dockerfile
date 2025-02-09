FROM rocker/shiny:4.3.0

LABEL org.opencontainers.image.title="geoflow-shiny"
LABEL org.opencontainers.image.url="https://github.com/r-geoflow/geoflow-shiny"
LABEL org.opencontainers.image.source="https://github.com/r-geoflow/geoflow-shiny"
LABEL org.opencontainers.image.description="A shiny app to configure and run geoflows"
LABEL org.opencontainers.image.authors="Emmanuel Blondel <eblondel.pro@gmail.com>"

# system libraries for LaTeX reporting & keyring
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    texlive-xetex \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-formats-extra \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    libsodium-dev \
    libsecret-1-dev \
    librdf0 \
    librdf0-dev
    
# general system libraries
# Note: this includes rdf/redland system libraries
RUN apt-get update && apt-get install -y \
    cmake \
    curl \
    default-jdk \
    fonts-roboto \
    ghostscript \
    hugo \
    less \
    libbz2-dev \
    libglpk-dev \
    libgmp3-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libhunspell-dev \
    libicu-dev \
    liblzma-dev \
    libmagick++-dev \
    libopenmpi-dev \
    libpcre2-dev \
    libssl-dev \
    libv8-dev \
    libxml2-dev \
    libxslt1-dev \
    libzmq3-dev \
    lsb-release \
    qpdf \
    texinfo \
    software-properties-common \
    vim \
    wget
    
RUN install2.r --error --skipinstalled --ncpus -1 redland
RUN apt-get install -y \
    libcurl4 \
    libgit2-dev \
    libxslt-dev \
    librdf0 \
    redland-utils \
    rasqal-utils \
    raptor2-utils
    
#geospatial libraries install
RUN /rocker_scripts/install_geospatial.sh

# install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 httpuv

#working directory
WORKDIR /srv/geoflow-shiny

# Set environment variables for renv cache, see doc https://docs.docker.com/build/cache/backends/
ARG RENV_PATHS_ROOT

# Make a directory in the container
RUN mkdir -p ${RENV_PATHS_ROOT}

#copy renv configuration
RUN R -e "install.packages(c('renv'), repos='https://cran.r-project.org/')"
COPY renv.lock renv.lock
COPY .Rprofile  .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Set renv cache location: change default location of cache to project folder
# see documentation for Multi-stage builds => https://cran.r-project.org/web/packages/renv/vignettes/docker.html
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE=renv/.cache

# Restore the R environment
RUN R -e "renv::restore()"

#copy app
COPY . /srv/geoflow-shiny
#etc dirs (for config)
RUN mkdir -p /etc/geoflow-shiny/

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/srv/geoflow-shiny',port=3838,host='0.0.0.0')"]
