# AUC and AUPR calculation for CH baseline models at admin level
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
unit = "states"
type = "baseline"

conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))

conflicts %<>%
  filter(unit == "states", time > as.Date("2000-12-01"), id <= 847)

vars = c("all", "sb", "ns", "os")
for(var in vars){
  # split data to train, val and test
  data = split_data_baseline(conflicts, thres = 0, y_var = var, seed = seed)
  repeats = 1:10
  metrics <-  readRDS(file.path(envrmt$`env_test-results`, paste0("test-results-", type, "-", unit, "-", var, ".rds")))
  results <- lapply(repeats, function(rep){
    model <- load_model_hdf5(file.path(envrmt$env_models, paste0("repeat-", rep, "-model-", type, "-", unit, "-", var, ".h5")))
    pred <- c(predict(model, data$test$x))
    obsv <- c(data$test$y)
    aucpr = PRROC::pr.curve(pred[obsv == 1], pred[obsv == 0], curve = T)
    auc = PRROC::roc.curve(pred[obsv == 1], pred[obsv == 0], curve = T)
    df = data.frame(obsv = obsv, pred = pred, y_hat = if_else(pred>=filter(metrics, rep == !!rep, name == "threshold")%>%pull(score),1,0))
    df$obsv = factor(df$obsv, levels = c(0, 1), labels = c("peace", "conflict"))
    df$y_hat = factor(df$y_hat, levels = c(0, 1), labels = c("peace", "conflict"))
    df$type = type
    df$unit = unit
    df$var = var
    df$month = rep(1:12, nrow(df)/12)
    
    cnf = confusionMatrix(data = df$y_hat, reference = df$obsv, positive = "conflict", mode = "prec_recall")
    list(auc = auc, aucpr = aucpr, df = df, cnf = cnf, rep = rep)
  }) 
  saveRDS(results, file = file.path(envrmt$env_acc, paste0(paste("acc", type, unit, var, sep = "-"), ".rds")))
}

