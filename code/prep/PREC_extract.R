# Extraction of monthly precipitation amounts
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

files = list.files("data/raw/chirps/", pattern = ".tif.gz$", full.names = T) # collect files
aoi = st_read("data/vector/states_mask.gpkg") # read extent shapefile
aoi = st_transform(aoi, st_crs("EPSG:4326"))

# set options for gdalcubes
gdalcubes_options(threads = 1, cache = TRUE, debug = TRUE)
# create collection DB if non existing
if(!file.exists("data/DB/chirpsDB.db")){
DB = create_image_collection(files, format = "CHIRPS_v2_0_monthly_p05_tif", out_file = "data/DB/chirpsDB.db")
} else {
DB = image_collection("data/DB/chirpsDB.db")
}

extent = extent(DB)
bbox = st_bbox(aoi)
extent$left = bbox[1]
extent$right = bbox[3]
extent$top = bbox[4]
extent$bottom = bbox[2]

view = cube_view(extent = extent, 
                 srs = "EPSG:4326",
                 dt = "P1M", 
                 dx = 0.05, # native resoultion
                 dy = 0.05,
                 aggregation = "mean",
                 resampling = "bilinear")

raster_cube(DB, view, chunking = c(1, 1024, 1204)) %>%
  #select_time(t = "2014-10") %>%
  write_tif(dir = "data/raster/chirps/", prefix = "prec_",
            creation_options = list("COMPRESS" = "LZW"))
