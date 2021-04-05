# Calculation of precipitation anomalies based on CHIRPS data set
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

if(!file.exists("data/raw/chirps/chirps-v2.0.monthly.nc")){
  download.file("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc",
                destfile = "data/raw/chirps/chirps-v2.0.monthly.nc")
}

# convert nc file to GTiffs so analysis with gdalcubes is possible (see 001_analysis.R)
chirps_raster = brick("data/raw/chirps/chirps-v2.0.monthly.nc")
dates = str_sub(names(chirps_raster), 2, 8) 
filenames = paste("data/raw/chirps/chirps-v2.0.", dates, ".tif", sep = "")
writeRaster(chirps_raster, filename = filenames, bylayer = TRUE, overwrite = FALSE)

aoi = st_read("data/vector/states_mask.gpkg")
bbox = st_bbox(aoi)
files = list.files("data/raw/chirps/", pattern =".tif$", full.names = T)
dates =  as.Date(paste(dates, ".01", sep  =""), format = "%Y.%m.%d")
col = create_image_collection(files, date_time = dates, band_names = "precipitation")
gdalcubes_options(threads = 4, debug = T)
# make custom extent
ext = gdalcubes::extent(col)
ext$left = bbox[1]
ext$right = bbox[3]
ext$top = bbox[4]
ext$bottom = bbox[2]
ext$t1 = "2019-12-31T00:00:00"
# create cube view
view_monthly = cube_view(extent = ext,
                         dx = 0.05, dy = 0.05, dt = "P1M",
                         aggregation = "mean", resampling = "bilinear",
                         srs = "EPSG:4326")


target_months = as.character(seq(as.Date("2000-01-01"), 
                                 as.Date("2019-12-31"), 
                                 by = "months"))

# calculate monthly anomalies
raster_cube(col, view_monthly, chunking = c(468, 250, 250)) %>%
  apply_time(names= "anomalie", FUN = function(x){
    y = x["precipitation",]
    result = rep(NA, length(y))
    if(all(is.na(y))){
      return(result)
    } else {
      z = matrix(y[1:360], ncol = 12, byrow = TRUE)
      z = colMeans(z)
      z = rep(z, (length(y)/12))
      result[] = y - z
      return(result)
    }
  }) %>%
  select_time(target_months) %>%
  write_tif(dir = "data/raster/anom/", prefix = "anom_", 
            creation_options = list("COMPRESS"= "LZW"))
