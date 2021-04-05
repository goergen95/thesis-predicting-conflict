# Extraction of daily GPP raster sets 
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

files = list.files("data/raw/gpp/", pattern = ".hdf$", full.names = T) # collect files
aoi = st_read("data/vector/states_mask.gpkg") # read extent shapefile
aoi = st_transform(aoi, st_crs("EPSG:4326"))

# set options for gdalcubes
gdalcubes_options(threads = 8, cache = TRUE, debug = T)
# create collection DB if non existing
if(!file.exists("data/DB/MD17DB.db")){
  DB = create_image_collection(files, format = "../collection_formats/formats/MxD17A2HGF.json", out_file = "data/DB/MD17DB.db")
} else {
  DB = image_collection("data/DB/MD17DB.db")
}

extent = extent(DB)
bbox = st_bbox(aoi)
extent$left = bbox[1]
extent$right = bbox[3]
extent$top = bbox[4]
extent$bottom = bbox[2]

rm(aoi, bbox, files)
gc()
year = 2000:2019
for(y in year){
  days = seq(ymd(paste0(y, "-01-01")),ymd(paste0(y, "-12-31")), by = '1 day')
  for (i in 1:46){
    
    dates = days[(i*8-8+1) : (i*8)]
    dates = na.omit(dates)
    print(i)
    print(dates[1])
    extent$t0 = as.character(dates[1])
    extent$t1 = as.character(dates[length(dates)])
    formula = paste0("(GPP/", length(dates), ")")
    
    packing = pack_minmax(type = "int16", min = 0, max = 30000)
    
    view = cube_view(extent = extent, 
                     srs = "EPSG:4326",
                     dt = "P1D", 
                     dx = 0.01, # native resolution
                     dy = 0.01,
                     aggregation = "mean",
                     resampling = "sum")
    
    start_time = Sys.time()
    raster_cube(DB, view, mask = image_mask("GPP", values = 32761:32767),
                chunking = c(8, 4000, 4000)) %>%
      select_bands("GPP") %>%
      fill_time(method = "locf") %>%
      # select_time(c("2005-06-26")) %>%
      apply_pixel(formula, "GPP") -> cube 
    cube %>%
      write_tif(dir = "data/raster/gpp/", prefix = "gpp_", COG = F, pack = packing,
                creation_options = list("COMPRESS" = "LZW"))
    end_time = Sys.time()
    print(end_time - start_time)
    rm(cube, start_time, end_time, view, packing, formula, dates)
    gc()
  }
}
