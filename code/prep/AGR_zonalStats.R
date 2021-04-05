# Zonal Statistics extraction for agricultural interaction variables
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
ras_files = list.files("data/raster/agr/", full.names = T)

vars <- c("_et-", "spei", "spi", "prec", "gpp", "lst", "anom")
vars <- c("anom")
for(var in vars){
  
  tmp_files = ras_files[grep(var, ras_files)]
  
  if(var == "_et-"){
    band_names = "AGRET"
    filename = band_names
    res = 0.005
  }
  if(var == "spei"){
    band_names = c("AGRSPEI1", "AGRSPEI3", "AGRSPEI6", "AGRSPEI12")
    filename = "AGRSPEI"
    res = 0.05
  }
  if(var == "spi"){
    band_names = c("AGRSPI1", "AGRSPI3", "AGRSPI6", "AGRSPI12")
    filename = "AGRSPI"
    res = 0.05
  }
  if(var == "prec"){
    band_names = "AGRPREC"
    filename = band_names
    res = 0.005
  }
  if(var == "gpp"){
    band_names = "AGRGPP"
    filename = band_names
    res = 0.005
  }
  if(var == "lst"){
    band_names = "AGRLST"
    filename = band_names
    res = 0.005
  }
  if(var == "anom"){
    band_names = c("AGRANOM")
    filename = band_names
    res = 0.005
  }
  
  col = create_image_collection(tmp_files,
                                date_time =seq(as.Date("2000-01-01"),
                                               as.Date("2019-12-31"),
                                               by = "month"),
                                band_names = band_names)
  
  view = cube_view(extent = gdalcubes::extent(col),
                   dx = res, dy = res, dt = "P1M",
                   srs = "EPSG:4326", aggregation = "mean",
                   resampling = "near")
  
  # loop through unit files for zonal extraction
  for (i in 1:length(unit_files)){
    print(i)
    aoi = st_read(unit_files[i], quiet = T)
    aoi = st_transform(aoi, crs = st_crs("EPSG:4326"))
    name_split = str_split(basename(unit_files[i]), "_")[[1]]
    if(length(name_split) == 3){
      outname = file.path("data/vector/extraction", paste0(name_split[1],"_", name_split[2],"_", filename, ".gpkg"))
    } else {
      outname = file.path("data/vector/extraction", paste0(name_split[1],"_", filename, ".gpkg"))
    }
    if(file.exists(outname)){
      next
    } else {
      raster_cube(col, view, chunking = c(240, 1000, 1000)) %>%
        zonal_statistics(aoi, expr = paste("mean(", band_names, ")", sep = ""), out_path = outname)
    }
  }
}
