# This script downloads two different datasets from the FAO Gismanager API.
# It uses a package specifically written for this purpose which is available
# at GitHub only at www.github.com/goergen95/wapoR

if(!"wapoR" %in% installed.packages()[,1]){
  devtools::install_github("goergen95/wapoR")  
}

library(wapoR)
library(sf)
aoi <- st_read("data/vector/states.gpkg")


# dims = c("BF", "CT", "CH", "DK", "GT", "HO", "PG", "SH")
# 
# 
# wapoR::wapor_queryRaster("GLW", "DA", dimensions = list("LVSTK" = c("BF", "CT", "CH", "DK", "GT", "HO", "PG", "SH")),
#                          aoi = aoi,
#                          APIkey = Sys.getenv("wapor-key"),
#                          outdir = "data/raw/lvstk")


wapor_queryRaster(collection = "ASIS", 
                         product = "ASI_D", 
                         dimensions = list("SEASON" = c("S1", "S2"), "LAND" = c("P")), 
                         aoi = aoi,
                         begin = "2000-01-01",
                         end = "2020-01-01",
                         APIkey = "3c64f7d69ad44189204f720dc4bb4a7c41464daf1afc69f493fa5cab51a6caf9875f540a2cbf9a1e",
                         outdir = "data/raw/asi/")

