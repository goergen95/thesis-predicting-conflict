# Data cube creation based on all individually extracted variables
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
files = list.files("data/vector/extraction/", full.names = T)

data = mclapply(files, function(x){
  filename = str_split(basename(x), "_")[[1]]
  if(length(filename) == 3){
    unit = filename[1]
    buffer = as.numeric(filename[2])
    var = str_remove(filename[3], ".gpkg")
  } else {
    unit = filename[1]
    buffer = 0
    var =  str_remove(filename[2], ".gpkg")
  }
  
  layers = ogrListLayers(x)
  layers = layers[grep("attr_", layers)]
  data = do.call(cbind, lapply(layers, function(l){
    tmp = st_read(x, layer = l, quiet = TRUE)
    if(var == "DEP"){
      tmp$DEP = tmp$DEP_sum / tmp$SUP_sum * 100
      tmp$DEP[is.na(tmp$DEP)] = 0
      tmp[,1:2] = NULL
      names(tmp) = l
    } else if(var=="YBULGE"){
      tmp$YBULGE = tmp$YOUTH_sum / tmp$OTHER_sum * 100
      tmp$YBULGE[is.na(tmp$YBULGE)] = 0
      tmp[,1:2] = NULL
      names(tmp) = l
    } else if(var=="LCC"){
      tmp =  round( tmp / rowSums(tmp) * 100 , digits = 5)
      names(tmp) = paste(l, c("CROP", "BARE", "FOREST", "GRASS", "SHRUB", "URBAN", "WATER"), sep = "-")
    } else if(var=="SPEI"){
      lnames = c("SPEI1", "SPEI3", "SPEI6", "SPEI12")
      names(tmp) = paste(l, lnames, sep = "-")
    } else if(var=="SPI"){
      lnames = c("SPI1", "SPI3", "SPI6", "SPI12")
      names(tmp) = paste(l, lnames, sep = "-")
    } else if(var=="AGRSPEI"){
      lnames = c("AGRSPEI1", "AGRSPEI3", "AGRSPEI6", "AGRSPEI12")
      names(tmp) = paste(l, lnames, sep = "-")
    } else if(var=="AGRSPI"){
      lnames = c("AGRSPI1", "AGRSPI3", "AGRSPI6", "AGRSPI12")
      names(tmp) = paste(l, lnames, sep = "-")
    } else {
      names(tmp) = l
    }
    tmp
  }))
  
  dates = names(data)
  
  if(var %in% c("LCC", "GDP", "DEP", "YBULGE")){
    dates = as.numeric(str_sub(dates, 6, 9))
    data = data[,order(dates)]
  } else if (var %in% c("LVSTK", "TRT", "TRI")) {
    data = data[ ,rep(1, 20)]
    dates = sapply(0:19, function(y) if_else(y<10, paste0("200",y), paste0("20", y)))
    names(data) = paste("attr_", dates, sep = "")
  } else {
    dates = as.Date(paste0(str_sub(dates, 6, 12), "-01"))
    data = data[,order(dates)]
  }
  
  if(var %in% c("SPEI", "SPI")){
    data$id = 1:nrow(data)
    data %>%
      as_tibble () %>%
      gather("tmp", "value", -id) %>%
      mutate(var = str_sub(tmp, 14, -1),
             time = str_sub(tmp, 6, 12),
             unit = unit, buffer = buffer, var = var) %>%
      dplyr::select(-tmp) %>%
      relocate(id, unit, buffer, var, time, value) %>%
      arrange(id, time, unit, var, buffer)
    
  } else if(var %in% c("AGRSPEI", "AGRSPI")){
    data$id = 1:nrow(data)
    data %>%
      as_tibble () %>%
      gather("tmp", "value", -id) %>%
      mutate(var = str_sub(tmp, 14, -1),
             time = str_sub(tmp, 6, 12),
             unit = unit, buffer = buffer, var = var) %>%
      dplyr::select(-tmp) %>%
      relocate(id, unit, buffer, var, time, value) %>%
      arrange(id, time, unit, var, buffer)
    
  } else if(var == "GDP"){
    
    data$id = 1:nrow(data)
    data %>%
      as_tibble() %>%
      mutate() %>%
      gather("time", "value", -id) %>%
      mutate(time = str_remove(time, "attr_"),
             unit = unit, buffer = buffer, var = var) %>%
      left_join(data.frame(time = as.character(rep(2000:2019,each = 12)), 
                           month = rep(1:12, length(2000:2019)))) %>%
      mutate(time = paste0(time,"-", if_else(month<10,paste0("0",month), as.character(month)))) %>%
      dplyr::select(-month) %>%
      relocate(id, unit, buffer, var, time, value) %>%
      arrange(id, time, unit, var, buffer)
    
  } else if(var == "LCC"){
    
    data$id = 1:nrow(data)
    data %>%
      as_tibble() %>%
      mutate() %>%
      gather("time", "value", -id) %>%
      separate(time, into = c("time", "var"), sep = "-") %>%
      mutate(time = str_remove(time, "attr_"),
             unit = unit, buffer = buffer, var = var) %>%
      left_join(data.frame(time = as.character(rep(2000:2019,each = 12)), 
                           month = rep(1:12, length(2000:2019)))) %>%
      mutate(time = paste0(time,"-", if_else(month<10,paste0("0",month), as.character(month)))) %>%
      dplyr::select(-month) %>%
      relocate(id, unit, buffer, var, time, value) %>%
      arrange(id, time, unit, var, buffer)
    
    
  } else if(var %in% c("DEP", "YBULGE")){
    
    data$id = 1:nrow(data)
    data %>%
      as_tibble() %>%
      mutate() %>%
      gather("time", "value", -id) %>%
      mutate(time = str_remove(time, "attr_"),
             unit = unit, buffer = buffer, var = var) %>%
      left_join(data.frame(time = as.character(rep(2000:2019,each = 12)), 
                           month = rep(1:12, length(2000:2019)))) %>%
      mutate(time = paste0(time,"-", if_else(month<10,paste0("0",month), as.character(month)))) %>%
      dplyr::select(-month) %>%
      relocate(id, unit, buffer, var, time, value) %>%
      arrange(id, time, unit, var, buffer)
    
  } else if(var %in% c("LVSTK", "TRI", "TRT")){
    
    data$id = 1:nrow(data)
    data %>%
      as_tibble() %>%
      mutate() %>%
      gather("time", "value", -id) %>%
      mutate(time = str_remove(time, "attr_"),
             unit = unit, buffer = buffer, var = var) %>%
      left_join(data.frame(time = as.character(rep(2000:2019,each = 12)), 
                           month = rep(1:12, length(2000:2019)))) %>%
      #expand(nesting(id, unit, buffer, var, value, month), time = full_seq(2000:2019,1)) %>%
      mutate(time = paste0(time,"-", if_else(month<10,paste0("0",month), as.character(month)))) %>%
      dplyr::select(-month) %>%
      relocate(id, unit, buffer, var, time, value) %>%
      arrange(id, time, unit, var, buffer)
    
  } else {
    data$id = 1:nrow(data)
    data %>%
      as_tibble() %>%
      mutate(unit = unit, buffer = buffer, var = var) %>%
      gather("time", "value", -id, -unit, -buffer, -var) %>%
      mutate(time = str_remove(time, "attr_")) %>%
      arrange(id, time, unit, var, buffer)
  }
}, mc.cores = 6)

data = do.call(rbind, data)
unique(data$var)
data %>%
  filter(buffer == 0) %>%
  group_by(unit, var) %>%
  summarise(skew = skewness(value, na.rm =T)) -> skew_summary

data %>%
  mutate(var = if_else(buffer>0, paste0(var, "_", buffer), var),
         time = as.Date(paste0(time, "-01"))) %>%
  dplyr::select(-buffer) %>%
  relocate(id, time, unit, var) %>%
  # arrange(id, time, unit, var) %>%
  as.tbl_cube(dim_names = 1:4) -> data_cube

saveRDS(data_cube, "data/vector/predictor_cube.rds")


