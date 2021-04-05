# Zonal Stats for variable GDP
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

source("code/setup.R")
gdalcubes_options(threads = 8, debug = T)

unit_files = list.files("data/vector/", pattern = "mask", full.names = T)
ras_files = list.files("data/raster/gdp/", full.names = T)

col = create_image_collection(ras_files, date_time = seq(as.Date("2000-01-01"),
                                                         as.Date("2019-12-31"),
                                                         by = "year"), 
                              band_names = "GDP")
view = cube_view(extent = extent(col),
                 dx = 0.083, dy =  0.083, dt = "P1Y",
                 srs = "EPSG:4326", aggregation = "mean",
                 resampling = "near")

# loop through unit files for zonal extraction
for (i in 1:length(unit_files)){
  print(i)
  aoi = st_read(unit_files[i])
  name_split = str_split(basename(unit_files[i]), "_")[[1]]
  if(length(name_split) == 3){
    filename = file.path("data/vector/extraction", paste0(name_split[1],"_", name_split[2],"_GDP.gpkg"))
  } else {
    filename = file.path("data/vector/extraction", paste0(name_split[1],"_GDP.gpkg"))
  }
  
  raster_cube(col, view) %>%
    zonal_statistics(aoi, expr = "mean(GDP)", out_path = filename)
  
}
