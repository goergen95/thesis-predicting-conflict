# Interaction variable extraction between EV predictors and agri mask
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

files_lcc = list.files("data/raster/lcc", full.names = T, pattern = "cropland")
files = list.files("data/raster/gpp_m/", full.names = T)

years <- 2000:2019

for (y in years){
  print(paste0("Starting calculation of year ", y,"..."))
  if(y == 2000){
    lcc <- files_lcc[grep(paste0("_2001.tif"), basename(files_lcc))]
  } else {
    lcc <- files_lcc[grep(paste0("_",y, ".tif"), basename(files_lcc))]
  }
  vals <- files[grep(paste0("_", y), files)]
  for(val in vals){
    print(paste0("File ", which(val == vals), " out of ", length(vals)))
    tmpraster <- tempfile(fileext = ".tif")
    tmpwkt <- tempfile(fileext = ".wkt")
    outname = file.path(envrmt$env_agr, paste0("agr_gpp-", str_sub(basename(val), 5, 18)))
    # warping image to mask
    command <- paste0("gdalsrsinfo -o wkt ", lcc, " > ", tmpwkt)
    system(command)
    command <- paste0("gdalwarp -ot Int16 -t_srs ", tmpwkt, " -ts 13790 14434 ", val, " ", tmpraster)
    system(command)
    command <- paste0("gdal_calc.py -A ", lcc, " -B ", tmpraster, " --calc='(A*B)' --type=Int16 --co='COMPRESS=LZW' --outfile=", outname)
    system(command)
    file.remove(c(tmpraster, tmpwkt))
    # system(paste0("gdalinfo ", vals[2]))
  }
}


files = list.files("data/raster/chirps/", full.names = T)

for (y in years){
  print(paste0("Starting calculation of year ", y,"..."))
  if(y == 2000){
    lcc <- files_lcc[grep(paste0("_2001.tif"), basename(files_lcc))]
  } else {
    lcc <- files_lcc[grep(paste0("_",y, ".tif"), basename(files_lcc))]
  }
  vals <- files[grep(paste0("_", y), files)]
  for(val in vals){
    print(paste0("File ", which(val == vals), " out of ", length(vals)))
    tmpraster <- tempfile(fileext = ".tif")
    tmpwkt <- tempfile(fileext = ".wkt")
    outname = file.path(envrmt$env_agr, paste0("agr_prec-", str_sub(basename(val), 6, 18)))
    # warping image to mask
    command <- paste0("gdalsrsinfo -o wkt ", lcc, " > ", tmpwkt)
    system(command)
    command <- paste0("gdalwarp -ot Int16 -t_srs ", tmpwkt, " -ts 13790 14434 ", val, " ", tmpraster)
    system(command)
    command <- paste0("gdal_calc.py -A ", lcc, " -B ", tmpraster, " --calc='(A*B)' --type=Int16 --co='COMPRESS=LZW' --outfile=", outname)
    system(command)
    file.remove(c(tmpraster, tmpwkt))
  }
}

files = list.files("data/raster/anom/", full.names = T)

for (y in years){
  print(paste0("Starting calculation of year ", y,"..."))
  if(y == 2000){
    lcc <- files_lcc[grep(paste0("_2001.tif"), basename(files_lcc))]
  } else {
    lcc <- files_lcc[grep(paste0("_",y, ".tif"), basename(files_lcc))]
  }
  vals <- files[grep(paste0("_", y), files)]
  for(val in vals){
    print(paste0("File ", which(val == vals), " out of ", length(vals)))
    tmpraster <- tempfile(fileext = ".tif")
    tmpwkt <- tempfile(fileext = ".wkt")
    outname = file.path(envrmt$env_agr, paste0("agr_anom-", str_sub(basename(val), 6, 12), ".tif"))
    # warping image to mask
    command <- paste0("gdalsrsinfo -o wkt ", lcc, " > ", tmpwkt)
    system(command)
    command <- paste0("gdalwarp -ot Int16 -t_srs ", tmpwkt, " -ts 13790 14434 ", val, " ", tmpraster)
    system(command)
    command <- paste0("gdal_calc.py -A ", lcc, " -B ", tmpraster, " --calc='(A*B)' --type=Int16 --co='COMPRESS=LZW' --outfile=", outname)
    system(command)
    file.remove(c(tmpraster, tmpwkt))
  }
}

files = list.files("data/raster/et_m/", full.names = T)

for (y in years){
  print(paste0("Starting calculation of year ", y,"..."))
  if(y == 2000){
    lcc <- files_lcc[grep(paste0("_2001.tif"), basename(files_lcc))]
  } else {
    lcc <- files_lcc[grep(paste0("_",y, ".tif"), basename(files_lcc))]
  }
  vals <- files[grep(paste0("_", y), files)]
  for(val in vals){
    print(paste0("File ", which(val == vals), " out of ", length(vals)))
    tmpraster <- tempfile(fileext = ".tif")
    tmpwkt <- tempfile(fileext = ".wkt")
    outname = file.path(envrmt$env_agr, paste0("agr_et-", str_sub(basename(val), 4, 10), ".tif"))
    # warping image to mask
    command <- paste0("gdalsrsinfo -o wkt ", lcc, " > ", tmpwkt)
    system(command)
    command <- paste0("gdalwarp -dstnodata 65535 -ot Int16 -t_srs ", tmpwkt, " -ts 13790 14434 ", val, " ", tmpraster)
    system(command)
    command <- paste0("gdal_calc.py -A ", lcc, " -B ", tmpraster, " --calc='(A*B)' --type=UInt16 --co='COMPRESS=LZW' --outfile=", outname)
    system(command)
    file.remove(c(tmpraster, tmpwkt))
  }
}

files = list.files("data/raster/lst/", full.names = T)

for (y in years){
  print(paste0("Starting calculation of year ", y,"..."))
  if(y == 2000){
    lcc <- files_lcc[grep(paste0("_2001.tif"), basename(files_lcc))]
  } else {
    lcc <- files_lcc[grep(paste0("_",y, ".tif"), basename(files_lcc))]
  }
  vals <- files[grep(paste0("_", y), files)]
  for(val in vals){
    print(paste0("File ", which(val == vals), " out of ", length(vals)))
    tmpraster <- tempfile(fileext = ".tif")
    tmpwkt <- tempfile(fileext = ".wkt")
    outname = file.path(envrmt$env_agr, paste0("agr_lst-", str_sub(basename(val), 5, 11), ".tif"))
    # warping image to mask
    command <- paste0("gdalsrsinfo -o wkt ", lcc, " > ", tmpwkt)
    system(command)
    command <- paste0("gdalwarp -ot Int16 -t_srs ", tmpwkt, " -ts 13790 14434 ", val, " ", tmpraster)
    system(command)
    command <- paste0("gdal_calc.py -A ", lcc, " -B ", tmpraster, " --calc='(A*B)' --type=Int16 --co='COMPRESS=LZW' --outfile=", outname)
    system(command)
    file.remove(c(tmpraster, tmpwkt))
  }
}

files = list.files("data/raster/pet_m/", full.names = T)

for (y in years){
  print(paste0("Starting calculation of year ", y,"..."))
  if(y == 2000){
    lcc <- files_lcc[grep(paste0("_2001.tif"), basename(files_lcc))]
  } else {
    lcc <- files_lcc[grep(paste0("_",y, ".tif"), basename(files_lcc))]
  }
  vals <- files[grep(paste0("_", y), files)]
  for(val in vals){
    print(paste0("File ", which(val == vals), " out of ", length(vals)))
    tmpraster <- tempfile(fileext = ".tif")
    tmpwkt <- tempfile(fileext = ".wkt")
    outname = file.path(envrmt$env_agr, paste0("agr_pet-", str_sub(basename(val), 5, 11), ".tif"))
    # warping image to mask
    command <- paste0("gdalsrsinfo -o wkt ", lcc, " > ", tmpwkt)
    system(command)
    command <- paste0("gdalwarp -ot Int16 -t_srs ", tmpwkt, " -ts 13790 14434 ", val, " ", tmpraster)
    system(command)
    command <- paste0("gdal_calc.py -A ", lcc, " -B ", tmpraster, " --calc='(A*B)' --type=Int16 --co='COMPRESS=LZW' --outfile=", outname)
    system(command)
    file.remove(c(tmpraster, tmpwkt))
  }
}

files = list.files("data/raster/spei/", full.names = T)

for (y in years){
  print(paste0("Starting calculation of year ", y,"..."))
  if(y == 2000){
    lcc <- files_lcc[grep(paste0("_2001.tif"), basename(files_lcc))]
  } else {
    lcc <- files_lcc[grep(paste0("_",y, ".tif"), basename(files_lcc))]
  }
  vals <- files[grep(paste0("_", y), files)]
  for(val in vals){
    print(paste0("File ", which(val == vals), " out of ", length(vals)))
    tmpraster <- tempfile(fileext = ".tif")
    tmpwkt <- tempfile(fileext = ".wkt")
    outname = file.path(envrmt$env_agr, paste0("agr_spei-", str_sub(basename(val), 6, 12), ".tif"))
    # warping image to mask
    command <- paste0("gdalsrsinfo -o wkt ", val, " > ", tmpwkt)
    system(command, intern = T)
    # system(paste0("gdalinfo ", val ))
    command <- paste0("gdalwarp -r mode -ot Float64 -t_srs ", tmpwkt, " -ts 1379 1444 ", lcc, " ", tmpraster)
    system(command, intern = T)
    command <- paste0("gdal_calc.py -A ", val, " -B ", tmpraster, " --allBands=A --calc='(A*B)' --type=Float64 --co='COMPRESS=LZW' --outfile=", outname)
    system(command, intern = T)
    # system(paste0("gdalinfo ", outname))
    file.remove(c(tmpraster, tmpwkt))
  }
}

files = list.files("data/raster/spi/", full.names = T)

for (y in years){
  print(paste0("Starting calculation of year ", y,"..."))
  if(y == 2000){
    lcc <- files_lcc[grep(paste0("_2001.tif"), basename(files_lcc))]
  } else {
    lcc <- files_lcc[grep(paste0("_",y, ".tif"), basename(files_lcc))]
  }
  vals <- files[grep(paste0("_", y), files)]
  for(val in vals){
    print(paste0("File ", which(val == vals), " out of ", length(vals)))
    tmpraster <- tempfile(fileext = ".tif")
    tmpwkt <- tempfile(fileext = ".wkt")
    outname = file.path(envrmt$env_agr, paste0("agr_spi-", str_sub(basename(val), 5, 11), ".tif"))
    # warping image to mask
    command <- paste0("gdalsrsinfo -o wkt ", val, " > ", tmpwkt)
    system(command, intern = T)
    # system(paste0("gdalinfo ", val ))
    command <- paste0("gdalwarp -r mode -ot Float64 -t_srs ", tmpwkt, " -ts 1379 1444 ", lcc, " ", tmpraster)
    system(command, intern = T)
    command <- paste0("gdal_calc.py -A ", val, " -B ", tmpraster, " --allBands=A --calc='(A*B)' --type=Float64 --co='COMPRESS=LZW' --outfile=", outname)
    system(command, intern = T)
    # system(paste0("gdalinfo ", outname))
    file.remove(c(tmpraster, tmpwkt))
  }
}



