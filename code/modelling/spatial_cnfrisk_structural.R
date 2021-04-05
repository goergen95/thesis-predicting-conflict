# Spatial Mapping of averaged conflict risk over the testing period (SV)
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
type = "structural"
ncores = 1
conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))


for (unit in units){
  if(unit == "basins"){
    conflicts = readRDS("data/vector/response_cube.rds")
    predictors = readRDS("data/vector/predictor_cube.rds")
    
    conflicts %<>%
      filter(unit == "basins", time > as.Date("2000-12-01"))
    predictors %<>%
      filter(unit == "basins", time > as.Date("2000-12-01"))
  } 
  if(unit == "states"){
    conflicts = readRDS("data/vector/response_cube.rds")
    predictors = readRDS("data/vector/predictor_cube.rds")
    
    conflicts %<>%
      filter(unit == "states", time > as.Date("2000-12-01"), id <= 847)
    predictors %<>%
      filter(unit == "states", time > as.Date("2000-12-01"), id <= 847)
    
  }
  
    predictors %>% filter(id == 1, time == as.Date("2010-01-01")) %>% as_tibble() %>% pull(var) %>% unique %>% sort -> vars
    target_vars = vars[grep("DEP|GDP|BARE|CROP|FOREST|GRASS|SHRUB|URBAN|WATER|LVSTK|POP|TRI|TRT", vars)]
    log_vars = vars[grep("GDP|LVSTK|POP|TRI|TRT", vars)]
    
    predictors %>%
      filter(var %in% target_vars) -> predictors
    
    # take natural log for log vars
    predictors %<>%
      as_tibble() %>%
      mutate(value = if_else(var %in% log_vars, log(value), value)) %>%
      mutate(value = if_else(is.infinite(value), 0, value))
    
    # calculate normalistaion factors xmin and xmax based on data up to 2017
    predictors %>%
      filter(time > as.Date("2000-12-31"),
             time <= as.Date("2017-12-31")) %>%
      as_tibble() %>%
      dplyr::select(id, time, var, value) %>%
      group_by(var) %>%
      summarise(xmax = max(value, na.rm = T), xmin = min(value, na.rm=T)) -> norm_factors
    
    # apply normalization
    predictors %>%
      as_tibble() %>%
      dplyr::select(-unit) %>%
      left_join(norm_factors) %>%
      mutate(value = (value-xmin)/(xmax-xmin)) %>%
      dplyr::select(-xmax, -xmin) %>%
      mutate(value = tidyr::replace_na(value, -1)) %>%
      as.tbl_cube(dim_names = 1:3) -> predictors
  
  vars = c("all", "sb", "ns", "os")
  for(var in vars){
    data = split_data(data_pred = predictors, data_conf = conflicts, thres = 0, y_var = var, seed = seed)
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
