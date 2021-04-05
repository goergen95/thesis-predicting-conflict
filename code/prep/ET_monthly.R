# Combination of daily ET sets to monthly scale
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

files = list.files("data/raster/et/", full.names = TRUE, pattern = ".tif$")
date_time = str_sub(basename(files), 5, 14)
year = 2000:2019
month = 1:12
for (y in year){
  for (m in month){
    if(m<10) m = paste0("0", m)
    print(paste0(y,"-",m))
    tmp_files = files[grep(paste0(y,"-",m), files)]
    st =  stack(tmp_files)
    start_time = Sys.time()
    st = sum(st, na.rm = F)
    writeRaster(st, filename = paste0("data/raster/et_m/et_",y,"-",m,".tif"), 
                datatype = "INT2U", options="COMPRESS=LZW", overwrite = T)
    end_time = Sys.time()
    print(end_time - start_time)
    removeTmpFiles(h=0)
    rm(st, tmp_files)
    gc()
  }
}
