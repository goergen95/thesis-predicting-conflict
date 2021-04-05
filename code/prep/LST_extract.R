# Extraction of daily LST raster data sets
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

files = list.files("data/raw/temp/", pattern = ".hdf$", full.names = T) # collect files
aoi = st_read("data/vector/states_mask.gpkg") # read extent shapefile
aoi = st_transform(aoi, st_crs("EPSG:4326"))

# set options for gdalcubes
gdalcubes_options(threads = 4, cache = TRUE, debug = TRUE)
# create collection DB if non existing
if(!file.exists("data/DB/LSTDB.db")){
  DB = create_image_collection(files, format = "collection_formats/MxD11C3.json", out_file = "data/DB/LSTDB.db")
} else {
  DB = image_collection("data/DB/LSTDB.db")
}

extent = extent(DB)
bbox = st_bbox(aoi)
extent$left = bbox[1]
extent$right = bbox[3]
extent$top = bbox[4]
extent$bottom = bbox[2]
extent$t0 = "2000-01-01T00:00:00"
extent$t1 = "2019-12-31T00:00:00"

view = cube_view(extent = extent, 
                 srs = "EPSG:4326",
                 dt = "P1M", 
                 dx = 0.05, # native resoultion
                 dy = 0.05,
                 aggregation = "mean",
                 resampling = "near")

raster_cube(DB, view, image_mask("QC_Day", bits = 1, values = 0, invert = TRUE),
            chunking = c(30, 4000, 4000)) %>%
  select_bands("LST_Day_CMG") %>% 
  apply_pixel("(LST_Day_CMG*0.02)", "LST") %>%
  write_tif(dir = "data/raster/lst/", prefix = "LST_",
            creation_options = list("COMPRESS" = "LZW"))
