# Zonal Statistics for variable population numbers
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
gdalcubes_options(threads = 6, debug = T)

unit_files = list.files("data/vector/", pattern = "mask", full.names = T)
files = list.files("data/raw/pop/", pattern =".tif$", full.names = T)
col = create_image_collection(files, date_time = paste(2000:2020, "-01-01", sep = ""),
                              band_names = "POP")

# loop through unit files for zonal extraction
for (i in 1:length(unit_files)){
  print(i)
  aoi = st_read(unit_files[i])
  # extent 
  bbox = st_bbox(aoi)
  ext = extent(col)
  ext$left = bbox[1]
  ext$right = bbox[3]
  ext$bottom = bbox[2]
  ext$top = bbox[4]
  ext$t1 = "2019-12-31"
  
  view = cube_view(extent = ext,
                   dx = 0.0083, dy =  0.0083, dt = "P1M",
                   srs = "EPSG:4326", aggregation = "mean",
                   resampling = "near")
  
  name_split = str_split(basename(unit_files[i]), "_")[[1]]
  if(length(name_split) == 3){
    filename = file.path("data/vector/extraction", paste0(name_split[1],"_", name_split[2],"_POP.gpkg"))
  } else {
    filename = file.path("data/vector/extraction", paste0(name_split[1],"_POP.gpkg"))
  }
  
  raster_cube(col, view, chunking = c(240, 1000, 1000)) %>%
    fill_time(method = "locf") %>%
    zonal_statistics(aoi, expr = "sum(POP)", out_path = filename)
  
}
