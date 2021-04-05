# Download script and translation of SRTM DEM 
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

tiles = st_read("data/vector/srtm/tiles.shp")
aoi = st_read("data/vector/states_mask.gpkg")

intersects = st_intersects(tiles, aoi, sparse = FALSE)
intersects = which(apply(intersects,1,any))
tiles = tiles[intersects, ]

srtm_list = list()
for(i in 1:nrow(tiles)) {
  print(i)
  lon <- extent(tiles[i,])[1]  + (extent(tiles[i,])[2] - extent(tiles[i,])[1]) / 2
  lat <- extent(tiles[i,])[3]  + (extent(tiles[i,])[4] - extent(tiles[i,])[3]) / 2

  tile <- getData('SRTM',
                  lon=lon,
                  lat=lat,
                  path = "data/raw/srtm")

  srtm_list[[i]] <- tile
}

# using gdal to create a mosaic raster

command = "gdalbuildvrt dat/raw/srtm/mosaic.vrt data/raw/srtm/*.tif"
system(command)

command = 'gdal_translate -of GTiff -co "COMPRESS=LZW" -co "BIGTIFF=YES" data/raw/srtm/mosaic.vrt data/raw/srtm/srtm_africa.tif'
system(command)

# using gdaldem to calculate Terrain Ruggedness Index (TRI)
command = 'gdaldem TRI  -co "COMPRESS=LZW" -co "BIGTIFF=YES" data/raw/srtm/srtm_africa.tif  data/raster/srtm/tri_africa.tif'
system(command)
