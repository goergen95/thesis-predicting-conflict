# Zonal Statistics of variable Youth Bulge
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
gdalcubes_options(threads = 2, debug = T, cache = T)

unit_files = list.files("data/vector/", pattern = "mask", full.names = T)
unit_files = unit_files[-grep("response", unit_files)]
ras_files = list.files("data/raster/ybulge/", full.names = T)

col = create_image_collection(ras_files, date_time =seq(as.Date("2000-01-01"),
                                                        as.Date("2019-12-31"),
                                                        by = "year"),
                              band_names = c("YOUTH", "OTHER"))

view = cube_view(extent = gdalcubes::extent(col),
                 dx = 0.0083, dy = 0.0083, dt = "P1Y",
                 srs = "EPSG:4326", aggregation = "mean",
                 resampling = "near")

# loop through unit files for zonal extraction
for (i in 1:length(unit_files)){
  print(i)
  aoi = st_read(unit_files[i])
  name_split = str_split(basename(unit_files[i]), "_")[[1]]
  if(length(name_split) == 3){
    filename = file.path("data/vector/extraction", paste0(name_split[1],"_", name_split[2],"_YBULGE.gpkg"))
  } else {
    filename = file.path("data/vector/extraction", paste0(name_split[1],"_YBULGE.gpkg"))
  }
  
  raster_cube(col, view, chunking = c(240, 1000, 1000)) %>%
    fill_time("locf") %>%
    zonal_statistics(aoi, expr = c("sum(YOUTH)", "sum(OTHER)"), out_path = filename)
}
