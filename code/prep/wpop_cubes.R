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

source("src/setup.R")
gdalcubes_options(debug = T, threads = 8)


if(!file.exists("data/DB/wpopAgeDB.db")){
  files = list.files("data/raw/age/", pattern = ".tif", full.names = TRUE)
  DB = create_image_collection(files, format = "collection_formats/worldpop_ages.json", out_file = "data/DB/wpopAgeDB.db")
} else {
  DB = image_collection("data/DB/wpopAgeDB.db")
}

# files = list.files("data/raw/age/", pattern = ".tif", full.names = T)
# files = files[grep("2000", files)]
# DB = create_image_collection(files, format = "../collection_formats/formats/worldpop_ages.json")
# 

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
                 dt = "P1M", 
                 dx = 0.00833, # native resoultion
                 dy = 0.00833,
                 aggregation = "mean",
                 resampling = "near")

age_groups = c(0, 1, 5, 10, 15, 65, 70, 75, 80)
sexes = c("f", "m")
bands1 = paste(sexes[1], age_groups, sep ="_")
bands2 = paste(sexes[2], age_groups, sep ="_")
dep_bands = c(bands1, bands2)
age_groups = c(20, 25, 30, 35, 40, 45, 50, 60)
sexes = c("f", "m")
bands1 = paste(sexes[1], age_groups, sep ="_")
bands2 = paste(sexes[2], age_groups, sep ="_")
work_bands = c(bands1, bands2)


raster_cube(DB, view, chunking = c(240, 1000, 1000)) %>%
  fill_time("locf") %>%
  apply_pixel(names = c("deps", "works"), expr = 
                c(
                  paste0("(", paste(dep_bands, collapse = " + "), ")"),
                  paste0("(", paste(work_bands, collapse = " + "), ")")
                )
  ) %>%
  write_tif(dir = "data/raster/dep_ratio", prefix = "depratio_",
            creation_options = list("COMPRESS" = "LZW"))





# if(is.na(x[1])) return(c(NA, NA))
# 
# age_groups = c(0, 1, 5, 10, 15, 65, 70, 75, 80)
# sexes = c("f", "m")
# bands1 = paste(sexes[1], age_groups, sep ="_")
# bands2 = paste(sexes[2], age_groups, sep ="_")
# dep_bands = c(bands1, bands2)
# 
# age_groups = c(20, 25, 30, 35, 40, 45, 50, 60)
# sexes = c("f", "m")
# bands1 = paste(sexes[1], age_groups, sep ="_")
# bands2 = paste(sexes[2], age_groups, sep ="_")
# work_bands = c(bands1, bands2)
# 
# y = c(sum(x[dep_bands]), sum(x[work_bands]))
# return(y)
