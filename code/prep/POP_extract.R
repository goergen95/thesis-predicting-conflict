# Extraction of population number raster data sets
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

aoi = st_read("data/vector/states_mask.gpkg")
bbox = st_bbox(aoi)

files = list.files("data/raw/pop/", pattern =".tif$", full.names = T)
col = create_image_collection(files, date_time = paste(2000:2020, "-01-01", sep = ""),
                              band_names = "POP")
ext = extent(col)
ext$left = bbox[1]
ext$right = bbox[3]
ext$bottom = bbox[2]
ext$top = bbox[4]
ext$t1 = "2019-12-31"

view = cube_view(dx =0.0083, dy = 0.0083, dt = "P1M",
                 aggregation = "mean", resampling = "near",
                 srs = "EPSG:4326", extent = ext)

gdalcubes_options(threads = 6, debug = T, cache = TRUE)
raster_cube(col, view, chunking = c(240, 1000, 1000)) %>%
  fill_time(method = "locf") %>% 
  write_tif(dir = "data/raster/pop/", prefix = "pop_",
            creation_options = list("COMPRESS" = "LZW"))
