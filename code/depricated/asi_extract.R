# extract lvst

library(gdalcubes)
library(sf)
library(magrittr)

files = list.files("data/raw/asi/", pattern = ".tif$", full.names = T) # collect files
aoi = st_read("data/vector/states_mask.gpkg") # read extent shapefile
aoi = st_transform(aoi, st_crs("EPSG:4326"))

# set options for gdalcubes
gdalcubes_options(threads = 8, cache = TRUE, debug = TRUE)
# create collection DB if non existing
if(!file.exists("data/DB/asi.db")){
  DB = create_image_collection(files, format = "collection_formats/fao_ASI.json", out_file = "data/DB/asi.db")
} else {
  DB = image_collection("data/DB/asi.db")
}

extent = extent(DB)
bbox = st_bbox(aoi)
extent$left = bbox[1]
extent$right = bbox[3]
extent$top = bbox[4]
extent$bottom = bbox[2]
extent$t0 = "2000-01-01"
extent$t1 = "2019-12-31"

view = cube_view(extent = extent, 
                 srs = "EPSG:4326",
                 dt = "P1M", 
                 dx = 0.01, 
                 dy = 0.01,
                 aggregation = "mean",
                 resampling = "bilinear")

raster_cube(DB, view) %>%
  fill_time(method = "locf") %>%
  # apply_pixel("(BF + CH + CT + DK + GT + PG + SH)", "lvstk") +
  write_tif(dir = "data/raster/lvstk/", prefix = "lvstk_",
            creation_options = list("COMPRESS" = "LZW"))
