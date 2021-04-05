# Linear Interpolation of missing years for GDP
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

gdp = brick("data/raw/gdp/GDP_per_capita_PPP_1990_2015_v2.nc")
aoi = st_read("data/vector/states_mask.gpkg")
gdp = crop(gdp, aoi)
gdp = gdp[[11:26]] # select 2000 to 2015
suffixes = 2000:2015
writeRaster(gdp, filename = "data/raw/gdp/gdp.tif", bylayer = TRUE, suffix = suffixes)

# list yearly tif files
files = list.files("data/raw/gdp/", pattern = ".tif$", full.names = T)
gdp_stack = stack(files)
# build a linear regression model per pixel
# extract intercept, slope and pvalue
mod_stack = calc(gdp_stack, fun = function(x){
  if(is.na(x[1])){return(c(NA, NA, NA))}
  dat = data.frame(x = x, time = 1:16)
  mod = lm(x~time, data = dat)
  stats = summary(mod)$coefficients
  interc = stats[1,1]
  slope = stats[2,1]
  pvalue = stats[2,4]
  return(c(interc, slope, pvalue))
})
writeRaster(mod_stack, "data/raw/gdp/mod.tif")

x_stack = mod_stack
x_stack[!is.na(x_stack)] = 0
x_stack[[4]] = x_stack[[3]]
names(x_stack) =  2016:2019
x_stack[[1]][x_stack[[1]] == 0] = 17
x_stack[[2]][x_stack[[2]] == 0] = 18
x_stack[[3]][x_stack[[3]] == 0] = 19
x_stack[[4]][x_stack[[4]] == 0] = 20

gdp_predicted = mod_stack[[1]] + (mod_stack[[2]] * x_stack)

gdp_complete = stack(gdp_stack, gdp_predicted)
names(gdp_complete) = paste("gdp_", 2000:2019, sep = "")
suffixes = 2000:2019
writeRaster(gdp_complete, filename = "data/raster/gdp/gdp.tif", bylayer = TRUE, suffix = suffixes)
