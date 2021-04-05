# Extraction of daily PET raster data sets
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

files = list.files("data/raw/et/", pattern = ".hdf$", full.names = T) # collect files
aoi = st_read("data/vector/states_mask.gpkg") # read extent shapefile
aoi = st_transform(aoi, st_crs("EPSG:4326"))

# set options for gdalcubes
gdalcubes_options(threads = 8, cache = TRUE, debug = F)
# create collection DB if non existing
if(!file.exists("data/DB/MD16DB.db")){
  DB = create_image_collection(files, format = "../collection_formats/formats/MxD16A2GF.json", out_file = "data/DB/MD16DB.db")
} else {
  DB = image_collection("data/DB/MD16DB.db")
}

extent = extent(DB)
bbox = st_bbox(aoi)
extent$left = bbox[1]
extent$right = bbox[3]
extent$top = bbox[4]
extent$bottom = bbox[2]

rm(aoi, bbox, files)
gc()
year = 2002:2019
for(y in year){
  days = seq(ymd(paste0(y, "-01-01")),ymd(paste0(y, "-12-31")), by = '1 day')
  for (i in 1:46){
    
    dates = days[(i*8-8+1) : (i*8)]
    dates = na.omit(dates)
    print(i)
    print(dates[1])
    extent$t0 = as.character(dates[1])
    extent$t1 = as.character(dates[length(dates)])
    formula = paste0("((PET/", length(dates), ")*0.1)")
    
    packing = pack_minmax(type = "int16", min = -32767, max = 32700)
    
    view = cube_view(extent = extent, 
                     srs = "EPSG:4326",
                     dt = "P1D", 
                     dx = 0.05, # native resolution
                     dy = 0.05,
                     aggregation = "mean",
                     resampling = "sum")
    
    start_time = Sys.time()
    raster_cube(DB, view, mask = image_mask("PET", values = 32761:32767),
                chunking = c(8, 4000, 4000)) %>%
      select_bands("PET") %>%
      fill_time(method = "locf") %>%
      # select_time(c("2005-06-26")) %>%
      apply_pixel(formula, "PET") -> cube 
    cube %>%
      write_tif(dir = "data/raster/pet/", prefix = "pet_", COG = F, pack = packing,
                creation_options = list("COMPRESS" = "LZW"))
    end_time = Sys.time()
    print(end_time - start_time)
    rm(cube, start_time, end_time, view, packing, formula, dates)
    gc()
  }
}
