# Extraction of daily livestock raster data set
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

files = list.files("data/raw/lvstk/", pattern = ".tif$", full.names = T) # collect files
aoi = st_read("data/vector/states_mask.gpkg") # read extent shapefile
aoi = st_transform(aoi, st_crs("EPSG:4326"))

# set options for gdalcubes
gdalcubes_options(threads = 8, cache = TRUE, debug = TRUE)
# create collection DB if non existing
if(!file.exists("data/DB/lvstkDB.db")){
  DB = create_image_collection(files, format = "../collection_formats/formats/lvstk.json", out_file = "data/DB/lvstkDB.db")
} else {
  DB = image_collection("data/DB/lvstkDB.db")
}

extent = extent(DB)
bbox = st_bbox(aoi)
extent$left = bbox[1]
extent$right = bbox[3]
extent$top = bbox[4]
extent$bottom = bbox[2]
extent$t0 = "2000-01-01"
extent$t1 = "2019-12-31"

view = cube_view(extent = extent, 
                 srs = "EPSG:4326",
                 dt = "P1M", 
                 dx = 0.08, # native resoultion
                 dy = 0.08,
                 aggregation = "mean",
                 resampling = "bilinear")

# writting out once since all data is the same
raster_cube(DB, view) %>%
  fill_time(method = "locf") %>%
  select_time("2000-01-01") %>%
  apply_pixel("(BF + CH + CT + DK + GT + PG + SH)", "lvstk") %>%
  write_tif(dir = "data/raster/lvstk/", prefix = "lvstk_",
            creation_options = list("COMPRESS" = "LZW"))
  
