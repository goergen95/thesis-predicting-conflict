# Extraction of Land Cover Classification
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

source("code/setup.R")
outpath = "data/raster/lcc/"

chunking = c(1, 6000, 6000)
aoi = st_read("data/vector/states_mask.gpkg")
# aoi = aoi[which(aoi$iso_a2 == "UG"), ]
# aoi = aoi[40,]
bbox = st_bbox(aoi)

files = list.files("data/raw/lcc", full.names = T, pattern = ".hdf$")
if(!file.exists("data/DB/lccDB.db")){
  DB = create_image_collection(files, format = "collection_formats/MCD12Q1.json", out_file = "data/DB/lccDB.db")
} else {
  DB = image_collection("data/DB/lccDB.db")
}

ext = gdalcubes::extent(DB)
ext$left = bbox[1]
ext$right = bbox[3]
ext$bottom = bbox[2]
ext$top = bbox[4]
# ext$t1 = "2001-01-01T00:00:00"

pack = pack_minmax(type="uint8", min=0, max=1, simplify = T)
pack$offset = 0
pack$scale = 1
pack$nodata = 255

gdalcubes_options(threads = 6, debug = T)
view = cube_view(extent = ext,
                 dt = "P1Y", dx = 0.005, dy = 0.005,
                 aggregation = "first",
                 resampling = "near",
                 srs = "EPSG:4326")

raster_cube(DB, view, mask = image_mask("LW",values = 1), chunking = chunking) %>%
  select_bands(c("LC_Type1", "LC_Type2")) %>%
  apply_pixel(names = c("crops"), FUN = function(x){
    x1 = x["LC_Type1"]
    x2 = x["LC_Type2"]
    x1 = x1 %in% c(12, 14)
    x2 = x2 %in% c(12, 14)
    if(x1 == 255 | x2 == 255){
      return(NA)
    } else if(x1 & x2 ){
      return(1)
    } else {
      return(0)
    }
  }) %>%
  write_tif(dir = outpath, "cropland_", pack = pack, creation_options = list("COMPRESS" = "LZW"))


raster_cube(DB, view, mask = image_mask("LW",values = 1), chunking = chunking) %>%
  select_bands(c("LC_Type1", "LC_Type2")) %>%
  apply_pixel(names = c("crops"), FUN = function(x){
    x1 = x["LC_Type1"]
    x2 = x["LC_Type2"]
    x1 = x1 %in% c(1:5)
    x2 = x2 %in% c(1:5)
    if(x1 == 255 | x2 == 255){
      return(NA)
    } else if(x1 & x2 ){
      return(1)
    } else {
      return(0)
    }
  }) %>%
  write_tif(dir = outpath, "forest_", pack = pack, creation_options = list("COMPRESS" = "LZW"))


raster_cube(DB, view, mask = image_mask("LW",values = 1), chunking = chunking) %>%
  select_bands(c("LC_Type1", "LC_Type2")) %>%
  apply_pixel(names = c("crops"), FUN = function(x){
    x1 = x["LC_Type1"]
    x2 = x["LC_Type2"]
    x1 = x1 %in% c(13)
    x2 = x2 %in% c(13)
    if(x1 == 255 | x2 == 255){
      return(NA)
    } else if(x1 & x2 ){
      return(1)
    } else {
      return(0)
    }
  }) %>%
  write_tif(dir = outpath, "builtup_", pack = pack, creation_options = list("COMPRESS" = "LZW"))



raster_cube(DB, view, mask = image_mask("LW",values = 1), chunking = chunking) %>%
  select_bands(c("LC_Type1", "LC_Type2")) %>%
  apply_pixel(names = c("crops"), FUN = function(x){
    x1 = x["LC_Type1"]
    x2 = x["LC_Type2"]
    x1 = x1 %in% 10
    x2 = x2 %in% 10
    if(x1 == 255 | x2 == 255){
      return(NA)
    } else if(x1 & x2 ){
      return(1)
    } else {
      return(0)
    }
  }) %>%
  write_tif(dir = outpath, "grassland_", pack = pack, creation_options = list("COMPRESS" = "LZW"))


raster_cube(DB, view, mask = image_mask("LW",values = 1), chunking = chunking) %>%
  select_bands(c("LC_Type1", "LC_Type2")) %>%
  apply_pixel(names = c("crops"), FUN = function(x){
    x1 = x["LC_Type1"]
    x2 = x["LC_Type2"]
    x1 = x1 %in% c(6,7,8,9)
    x2 = x2 %in% c(6,7,8,9)
    if(x1 == 255 | x2 == 255){
      return(NA)
    } else if(x1 & x2 ){
      return(1)
    } else {
      return(0)
    }
  }) %>%
  write_tif(dir = outpath, "shrubland_", pack = pack, creation_options = list("COMPRESS" = "LZW"))



raster_cube(DB, view, mask = image_mask("LW",values = 1), chunking = chunking) %>%
  select_bands(c("LC_Type1", "LC_Type2")) %>%
  apply_pixel(names = c("crops"), FUN = function(x){
    x1 = x["LC_Type1"]
    x2 = x["LC_Type2"]
    x1 = x1 %in% c(16)
    x2 = x2 %in% c(15)
    if(x1 == 255 | x2 == 255){
      return(NA)
    } else if(x1 & x2 ){
      return(1)
    } else {
      return(0)
    }
  }) %>%
  write_tif(dir = outpath, "bare_", pack = pack, creation_options = list("COMPRESS" = "LZW"))

raster_cube(DB, view, chunking = chunking) %>%
  select_bands(c("LC_Type1", "LC_Type2")) %>%
  apply_pixel(names = c("crops"), FUN = function(x){
    x1 = x["LC_Type1"]
    x2 = x["LC_Type2"]
    x1 = x1 %in% c(11, 17)
    x2 = x2 %in% c(0, 11)
    if(x1 == 255 | x2 == 255){
      return(NA)
    } else if(x1 & x2 ){
      return(1)
    } else {
      return(0)
    }
  }) %>%
  write_tif(dir = outpath, "water_", pack = pack, creation_options = list("COMPRESS" = "LZW"))


files = list.files(outpath, full.names = T, pattern = ".tif$")

# prepare area raster
area_ras = raster(files[1])
area_ras = area(area_ras)
area_file = tempfile(pattern = "area_ras", fileext = ".tif")
writeRaster(area_ras, filename = area_file)


for (i in 1:length(files)){
  print(paste0("File ", i, " out of ", length(files)))
  filename = str_split(basename(files[i]), "_")[[1]]
  type = filename[1]
  year = filename[2]

  filename = file.path("data/raster/lcc_area", paste0(type, "_",year))
  command = paste0('gdal_calc.py --calc="(A*B)" --outfile=', filename,' -A ', files[i], ' -B ', area_file)
  system(command, intern = T)
}



