# AUC and AUPR calculation for EV models at basin level
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
unit = "basins"
type = "environmental"

conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))
predictors = readRDS(file.path(envrmt$env_vector, "predictor_cube.rds"))

conflicts %<>%
  filter(unit == "basins", time > as.Date("2000-12-01"))

predictors %<>%
  filter(unit == "basins", time > as.Date("2000-12-01"), !var %in% c("AGRANOM", "AGRANOM_50", "AGRANOM_100", "AGRANOM_200",
                                                                    "YBULGE", "YBULGE_50", "YBULGE_100", "YBULGE_200"))

predictors %>% filter(id == 1, time == as.Date("2010-01-01")) %>% as_tibble() %>% pull(var) %>% unique %>% sort -> vars
log_vars = vars[grep("GDP|LVSTK|POP|TRI|TRT", vars)]

# take natural log for log vars
predictors %>%
  as_tibble() %>%
  filter(var %in% log_vars) %>%
  mutate(value =  log(value)) %>%
  mutate(value = if_else(is.infinite(value), 0, value)) -> log_vals

predictors %<>%
  as_tibble() %>%
  filter(!var %in% log_vars) %>%
  rbind(log_vals)

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
  # split data to train, val and test
  data = split_data(data_pred = predictors, data_conf = conflicts, thres = 0, y_var = var, seed = seed)
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

