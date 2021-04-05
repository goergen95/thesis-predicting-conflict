# Calculation of variable Youth Bulge
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
gdalcubes_options(debug = T, threads = 6)


if(!file.exists("data/DB/wpopAgeDB.db")){
  files = list.files("data/raw/age/", pattern = ".tif", full.names = TRUE)
  DB = create_image_collection(files, format = "collection_formats/worldpop_ages.json", out_file = "data/DB/wpopAgeDB.db")
} else {
  DB = image_collection("data/DB/wpopAgeDB.db")
}


aoi = st_read("data/vector/states_mask.gpkg") # read extent shapefile
aoi = st_transform(aoi, st_crs("EPSG:4326"))
extent = gdalcubes::extent(DB)
bbox = st_bbox(aoi)
extent$left = bbox[1]
extent$right = bbox[3]
extent$top = bbox[4]
extent$bottom = bbox[2]
extent$t0 = "2000-01-01"
extent$t1 = "2019-12-31"

view = cube_view(extent = extent, 
                 srs = "EPSG:4326",
                 dt = "P1Y", 
                 dx = 0.0083, # native resoultion
                 dy = 0.0083,
                 aggregation = "mean",
                 resampling = "near")

age_groups = c(15, 20)
sexes = c("f", "m")
bands1 = paste(sexes[1], age_groups, sep ="_")
bands2 = paste(sexes[2], age_groups, sep ="_")
youth_bands = c(bands1, bands2)
age_groups = c(25, 30, 35, 40, 45, 50, 60, 65, 70, 75, 80)
sexes = c("f", "m")
bands1 = paste(sexes[1], age_groups, sep ="_")
bands2 = paste(sexes[2], age_groups, sep ="_")
other_bands = c(bands1, bands2)


raster_cube(DB, view, chunking = c(10, 500, 500)) %>%
  fill_time("locf") %>%
  apply_pixel(names = c("youth", "other"), expr = 
                c(
                  paste0("(", paste(youth_bands, collapse = " + "), ")"),
                  paste0("(", paste(other_bands, collapse = " + "), ")")
                )
  ) %>%
  write_tif(dir = "data/raster/ybulge", prefix = "ybulge_",
            creation_options = list("COMPRESS" = "LZW"))

