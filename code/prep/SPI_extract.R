# Calculation of monthly SPI
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

# aoi = st_read("data/vector/states_mask.gpkg")
# aoi = st_transform(aoi, st_crs("EPSG:4326"))
# aoi = aoi[100,]
# bbox = st_bbox(aoi)

prec_files = list.files("data/raster/chirps/", full.names = T)
date_times = str_sub(basename(prec_files), 6, 12)
date_times = paste(date_times, "-01", sep = "")
gdalcubes_options(debug = TRUE, threads = 8)
col = create_image_collection(prec_files, date_time = date_times, band_names = "prec")
ext = gdalcubes::extent(col)

# ext$left = bbox[1]
# ext$right = bbox[3]
# ext$top = bbox[4]
# ext$bottom = bbox[2]
view = cube_view(extent = ext,
                 dx = 0.05, dy = 0.05, dt = "P1M",
                 aggregation = "mean", resampling = "bilinear",
                 srs = "EPSG:4326")
start_time = Sys.time()
raster_cube(col, view, chunking = c(240, 250, 250)) %>%
  apply_time(names = c("spi_m1", "spi_m3", "spi_m6", "spi_m12"), FUN = function(x){
    library(SPEI)
    y = as.vector(x["prec",])
    y = ts(y, start = c(2000,1), frequency = 12)
    results = lapply(c(1,3,6,12), function(vals){
      c(spi(y, scale = vals, ref.start = c(2000,1), ref.end = c(2017,12), na.rm = T)$fitted)
    })
    results = do.call(rbind, results)
    # result = c(spi(y, scale = 1, ref.start = c(2000,1), ref.end = c(2017,12), na.rm = T)$fitted)
    return(results)
  }) %>%
  write_tif(dir = "data/raster/spi/", prefix = paste0("spi_"), #pack = packing,
            creation_options = list("COMPRESS" = "LZW"))
end_time = Sys.time()
print(end_time - start_time)

