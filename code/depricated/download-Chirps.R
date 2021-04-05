# This script is used to download the CHIRPS dataset using functionality
# from an R Package of the Environmental Informatics group of the 
# Faculty of Geography, University of Marburg. The files are downloaded as
# .tif.gz files with a global extent which can be directly read by gdalcubes

if(!"heavyRain" %in% installed.packages()[,1]){
  devtools::install_github("environmentalinformatics-marburg/heavyRain")  
}

library(heavyRain)

getCHIRPS(region = "global",
          format = "tifs",
          tres = "monthly",
          sres = 0.05,
          begin = as.Date("2002-09-01"),
          end = as.Date("2002-09-31"),
          dsn = "data/raw/chirps",
          cores = 2)


