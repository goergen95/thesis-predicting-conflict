# Zonal Statistics of variable SPI
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
gdalcubes_options(threads = 8, debug = T)

unit_files = list.files("data/vector/", pattern = "mask", full.names = T)
ras_files = list.files("data/raster/spi/", full.names = T)

col = create_image_collection(ras_files, date_time =seq(as.Date("2000-01-01"),
                                                        as.Date("2019-12-31"),
                                                        by = "month"),
                              band_names = c("SPI1", "SPI3", "SPI6", "SPI12"))
view = cube_view(extent = extent(col),
                 dx = 0.05, dy =  0.05, dt = "P1M",
                 srs = "EPSG:4326", aggregation = "mean",
                 resampling = "near")

# loop through unit files for zonal extraction
for (i in 1:length(unit_files)){
  print(i)
  aoi = st_read(unit_files[i])
  name_split = str_split(basename(unit_files[i]), "_")[[1]]
  if(length(name_split) == 3){
    filename = file.path("data/vector/extraction", paste0(name_split[1],"_", name_split[2],"_SPI.gpkg"))
  } else {
    filename = file.path("data/vector/extraction", paste0(name_split[1],"_SPI.gpkg"))
  }
  if(file.exists(filename)){
    next
  } else {
  raster_cube(col, view, chunking = c(240, 1000, 1000)) %>%
    zonal_statistics(aoi, expr = c("mean(SPI1)", "mean(SPI3)", "mean(SPI6)", "mean(SPI12)"), out_path = filename)
  }
}
