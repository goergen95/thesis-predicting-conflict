FROM rocker/verse:3.6.3
MAINTAINER "Darius Görgen" darius2402@web.de

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    lbzip2 \
    libgdal-dev \
    libgeos-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    liblwgeom-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libssl-dev \
    libudunits2-dev \
    libgit2-dev \
    netcdf-bin

ENV RENV_VERSION 0.12.3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /masterthesis
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
