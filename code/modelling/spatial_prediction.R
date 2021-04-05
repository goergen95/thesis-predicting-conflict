# Spatial Mapping of model with highest F2 Score
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

files = list.files("output/test-results/", pattern = ".gpkg", full.names = T)
acc_files = list.files(envrmt$`env_test-results`, full.names = T, pattern = "test-results")

types = c("baseline", "structural", "environmental")
units = c("basins", "states")
vars = c("all", "sb", "ns", "os")


# for(type in types){
#   for(unit in units){
#     for(var in vars){
#       
#       tmp = files[grep(paste(type, unit, var, sep = "-"), files)]
#       shape = st_read(tmp[1], quiet = T)
#       obsv = st_drop_geometry(shape)[grep("obsv", names(shape))]
#       
#       pred = obsv
#       pred[] = 0
#       
#       for(i in 1:length(tmp)){
#         y = st_read(tmp[i], quiet = T)
#         y = st_drop_geometry(y)[grep("pred", names(y))]
#         pred = pred + y
#       }
#       pred[pred<=5] = 0 
#       pred[pred>0] = 1
#     
#       pred$obsv.sum = NULL
#       names(pred) = paste("pred.m", 1:12, sep = ".")
#       pred$pred.sum = rowSums(pred)
#       shape[ ,names(pred)] = pred
#       filename = paste0("spatial-result-", paste(paste(type, unit, var, sep = "-")), ".gpkg")
#       st_write(shape, dsn = file.path(envrmt$`env_test-results`, filename), append = F)
#     }
#   }
# }



for(type in types){
  for(unit in units){
    for(var in vars){
      
      acc = readRDS(acc_files[grep(paste(type, unit, var, sep = "-"), acc_files)])
      acc = filter(acc, name == "f2", month == 0)
      rep = as.numeric(which(acc$score == max(acc$score, na.rm = T)))
      tmp = files[grep(paste(type, unit, var, sep = "-"), files)]
      tmp = tmp[grep(paste0("-", rep, "-"), tmp)]
      filename = paste0("spatial-result-", paste(paste(type, unit, var, sep = "-")), ".gpkg")
      file.copy(tmp, to = file.path(envrmt$`env_test-results`, filename), overwrite = T)
      
    }
  }
}
