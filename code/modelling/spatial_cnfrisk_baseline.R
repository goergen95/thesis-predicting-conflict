# Spatial Mapping of averaged conflict risk over the testing period (CH)
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
source("code/modelling/utils.R")
seed = 42
units = c("basins", "states")
type = "baseline"
ncores = 1
vars = c("all", "sb", "ns", "os")
conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))



for (unit in units){
  if(unit == "basins"){
    conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))
    conflicts %<>%
      filter(unit == "basins", time > as.Date("2000-12-01"))
  }
  
  if(unit == "states"){
    conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))
    conflicts %<>%
      filter(unit == "states", time > as.Date("2000-12-01"), id <= 847)
  }
  
  for(var in vars){
    data = split_data_baseline(conflicts, thres = 0, y_var = var, seed = seed)
    metrics = readRDS(file.path(envrmt$`env_test-results`, paste0("test-results-", type, "-", unit, "-", var, ".rds")))
    metrics %<>% filter(name == "f2", month == 0) 
    rep = as.numeric(which(metrics$score == max(metrics$score, na.rm = T)))
    model = load_model_hdf5(filepath = file.path(envrmt$env_models, paste0("repeat-", rep, "-model-", type, "-", unit, "-", var, ".h5")))
    pred = as_tibble(predict(model, data$test$x))
    names(pred) = paste("m", 1:12, sep = "")
    poly = st_read(file.path(envrmt$env_vector, paste0(unit, "_mask.gpkg")))
    pred$mean = rowMeans(pred)
    geom = st_geometry(poly)
    poly = cbind(st_drop_geometry(poly), pred)
    poly$geom = geom
    poly = st_as_sf(poly)
    obsv = rowSums(data$test$y)
    poly$obsv = obsv
    st_write(poly, file.path(envrmt$`env_test-results`, paste0(paste("spatial-cnfrisk",type,unit,var,sep="-"),".gpkg")), delete_dsn = TRUE)
  }
}
