packrat::on()
library(gdalcubes)
library(sf)
library(magrittr)
library(lubridate)
library(stringr)

files = list.files("data/raster/gpp/", full.names = TRUE, pattern = ".tif$")
date_time = str_sub(basename(files), 5, 14)

# set options for gdalcubes
gdalcubes_options(threads = 8, cache = TRUE, debug = T)
# create collection DB if non existing
if(!file.exists("data/DB/MD17DB_monthly.db")){
  DB = create_image_collection(files, format = "md17_daily.json", out_file = "data/DB/MD17DB_monthly.db")
} else {
  DB = image_collection("data/DB/MD17DB_monthly.db")
}


extent = extent(DB)
year = 2000:2019
month = 1:12


for (y in year){
  days = seq(ymd(paste0(y, "-01-01")),ymd(paste0(y, "-12-31")), by = '1 day')
  for (m in month){
    if(m<10) m = paste0("0", m)
    dates = as.character(days[grep(paste0(y,"-",m), days)])
    #dates = dates[]
    extent$t0 = dates[1]
    extent$t1 = dates[length(dates)]
    view = cube_view(extent = extent, 
                     srs = "EPSG:4326",
                     dt = "P1D", 
                     dx = 0.1, # native resolution
                     dy = 0.1,
                     aggregation = "mean",
                     resampling = "sum")
    
    packing = pack_minmax(type = "int16", min = 0, max = 30000)
    start_time = Sys.time()
    raster_cube(DB, view, 
                chunking = c(length(dates), 2500, 2500)) %>%
      fill_time(method = "linear") %>%
      reduce_time("sum(GPP)") %>%
      write_tif(dir = "data/raster/gpp_m/", prefix = "test_", COG = F, pack = packing,
                creation_options = list("COMPRESS" = "LZW"))
    end_time = Sys.time()
    print(end_time - start_time)
  }
}


for (i in 1:nlayers(st)){
  st[[i]] = is.na(st[[i]])
}
stt = sum(st)