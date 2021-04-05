# Setup script for the project structure
# Copyright (C) 2021 Darius A. Görgen
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>


if(!"devtools" %in% installed.packages()[,1])
  install.packages("devtools")
if(!"envimaR" %in% installed.packages()[,1])
  devtools::install_github("envima/envimaR")
if(!"kerastuneR"%in% installed.packages()[,1])
  devtools::install_github("goergen95/kerastuneR@bayesian-tuner")
if(!"gdalcubes"%in% installed.packages()[,1])
  devtools::install_github("applemar/gdalcubes_R")

libs = c("abind",
         "caret", 
         "cubelyr",
         "dplyr", 
         "envimaR",
         "gdalcubes", 
         "ggplot2", 
         "ggtext",
         "ggpubr",
         "Hmisc", 
         "keras", 
         "kerastuneR", 
         "lubridate", 
         "magrittr", 
         "MLeval", 
         "MLmetrics", 
         "parallel",
         "pROC", 
         "raster",
         "reticulate",
         "rgdal", 
         "sf",
         "stringr", 
         "spacetime", 
         "SPEI", 
         "stars", 
         "tensorflow",
         "tfaddons",
         "tmap",
         "tidyr", 
         "zoo")

loadandinstall = function(mypkg) {
  if (!is.element(mypkg, installed.packages()[,1]))
    install.packages(mypkg)
  library(mypkg, character.only = TRUE)}

for (lib in libs){loadandinstall(lib)}

# install deep learning dependencies
# keras::install_keras(method = "conda",
#                      conda = "auto",
#                      version = "2.4.3",
#                      tensorflow = "gpu",
#                      envname = "thesis",
#                      restart_session = FALSE)
# tensorflow::install_tensorflow(method = "conda",
#                                conda = "auto",
#                                version = "2.3.0-gpu",
#                                envname = "thesis",
#                                restart_session = F)
# #if(kerastuneR::keras_tuner_version() != "1.0.2"){
# kerastuneR::install_kerastuner(version = "1.0.2",
#                                restart_session = F,
#                                method = "conda",
#                                conda = "auto",
#                                envname = "thesis")
# #}
# tfaddons::install_tfaddons(method = "conda",
#                            version = "0.11.2",
#                            conda = "auto",
#                            envname = "thesis",
#                            restart_session = FALSE)
use_condaenv("thesis", required = T)
K <- keras::backend()
tfa = reticulate::import("tensorflow_addons")

# create environment variables
projRootDir = "."
dirs = c("data/", 
         "data/DB", 
         "data/raster", 
         "data/raster/age", "data/raster/agr", "data/raster/anom", "data/raster/chirps",
         "data/raster/dep_ratio", "data/raster/et", "data/raster/eth",
         "data/raster/et_m", "data/raster/gdp", "data/raster/gpp", 
         "data/raster/gpp_m", "data/raster/lcc", "data/raster/lcc_area",
         "data/raster/lst", "data/raster/lvstk", "data/raster/pet",
         "data/raster/pet_m", "data/raster/pop", "data/raster/spei",
         "data/raster/spi", "data/raster/srtm", "data/raster/worldpop", "data/raster/ybulge",
         "data/raw",
         "data/raw/age", "data/raw/chirps", "data/raw/et", "data/raw/eth",
         "data/raw/gdp", "data/raw/ged", "data/raw/gpp", "data/raw/hydrosheds",
         "data/raw/lcc", "data/raw/lvstk", "data/raw/npp", "data/raw/pop",
         "data/raw/srtm", "data/raw/lst", "data/raw/trt",
         "data/vector",
         "data/vector/extraction", "data/vector/gadm", "data/vector/ged",
         "data/vector/srtm",
         "output",
         "output/acc", "output/bayes", "output/ffs", "output/models", "output/test-results", "output/plots",
         "code",
         "code/prep",
         "code/modelling",
         "analysis")
envrmt = envimaR::createEnvi(root_folder = projRootDir,
                             folders = dirs, 
                             path_prefix = "env_")
