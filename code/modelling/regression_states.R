# Logistic Regression baseline on the admin level
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
type = "environmental"

conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))
predictors = readRDS(file.path(envrmt$env_vector, "predictor_cube.rds"))

conflicts %>%
  filter(unit == "states", time > as.Date("2000-12-01"), id <= 847) -> conflicts
predictors %>%
  filter(unit == "states", time > as.Date("2000-12-01"), id <= 847) -> predictors

predictors %>% filter(id == 1, time == as.Date("2010-01-01")) %>% as_tibble() %>% pull(var) %>% unique %>% sort -> vars
log_vars = vars[grep("GDP|LVSTK|POP|TRI|TRT", vars)]

# take natural log for log vars
predictors %>%
  as_tibble() %>%
  filter(var %in% log_vars) %>%
  mutate(value =  log(value)) -> log_vals
log_vals = do.call(tibble,lapply(log_vals, function(x) replace(x, is.infinite(x),NA)))

predictors %<>%
  as_tibble() %>%
  filter(!var %in% log_vars) %>%
  rbind(log_vals)


# calculate normalistaion factors xmin and xmax based on data up to 2017
predictors %>%
  filter(time > as.Date("2000-12-31"),
         time <= as.Date("2018-12-31")) %>%
  as_tibble() %>%
  dplyr::select(id, time, var, value) %>%
  group_by(var) %>%
  summarise(xmax = max(value, na.rm = T), xmin = min(value, na.rm=T)) -> norm_factors

# apply normalization
predictors %<>%
  as_tibble() %>%
  dplyr::select(-unit) %>%
  left_join(norm_factors) %>%
  mutate(value = (value-xmin)/(xmax-xmin)) %>%
  drop_na(value) %>%
  dplyr::select(-xmax, -xmin) 

predictors %>%
  pivot_wider(names_from = var) -> pred_data


vars = c("all", "sb", "ns", "os")
lags = 1:12

for(var in vars){
  print(paste0("Modelling variable ", var))
  results = list()
  for(l in lags){
    print(paste0("Starting modelling for lead time ", l))
    
    conflicts %>%
      as_tibble() %>%
      mutate(value = if_else(value > 0, 1, 0)) %>%
      mutate(value = factor(value, levels = c(0,1), labels = c("peace", "conflict"))) %>%
      pivot_wider(names_from = type) %>%
      dplyr::select(id, time, starts_with(var)) -> resp_data
    
    data <- left_join(pred_data, resp_data)
    data %>%
      pull(var) %>%
      lead(n = l) -> lead_response
    data[var] <- lead_response
    data %<>%
      drop_na()
    index_1 = which(data[var] == "conflict")
    index_0 = which(data[var] == "peace")
    
    folds = 10
    fold_results = list()
    
    fold_results = lapply(1:folds, function(k){
      print(paste0("Starting fold ", k, " of ", folds))
      set.seed(seed+k)
      index_0 = sample(index_0, length(index_1))
      data2 = data[c(index_0, index_1),]
      
      train <- data2 %>%
        filter(time <= as.Date("2018-12-01"))
      
      pred_vars <- names(train)[3:(ncol(train)-1)]
      response <- names(train)[ncol(train)]
      form <- paste0(response, " ~ ", paste(pred_vars, collapse = " + "))
      model <- glm(form, data = train, family = binomial)
      summary(model)
      
      test <- data %>%
        filter(time > as.Date("2018-12-01"))
      
      pred <- predict(model, test, type = "response")
      
      thresholds = seq(.001,1, 0.001)
      
      df = tibble(conflict = pred, peace = 1 - c(pred), obs = as.numeric(test[[var]]))
      df$obs = if_else(df$obs == 1, 0, 1)
      
      thres_results = mclapply(thresholds, function(thres){
        tmp = df
        tmp$pred = if_else(tmp$conflict<thres, 0, 1)
        if(length(unique(tmp$pred)) == 1) return(data.frame(thres = as.numeric(thres), score = NA))
        score = MLmetrics::FBeta_Score(y_true = tmp$obs, y_pred = tmp$pred, positive = 1, beta = 2)
        #score = MLmetrics::F1_Score(y_true=tmp$obs, y_pred = tmp$pred, positive = 1)
        tibble(thres = as.numeric(thres), score = as.numeric(score))
      }, mc.cores = 4)
      
      thres_results = do.call(rbind, thres_results)
      thres_results = thres_results[which(thres_results$score == max(thres_results$score, na.rm = TRUE)),]
      
      if(nrow(thres_results)>1){
        thres_results = thres_results[sample(nrow(thres_results), 1),]
      }
      
      threshold = thres_results$thres
      df$pred = if_else(df$conflict<threshold[1], 0, 1)
      bs = mean((df$conflict-df$obs)^2)
      f1 = MLmetrics::F1_Score(y_true = df$obs, y_pred = df$pred, positive = 1)
      f2 = MLmetrics::FBeta_Score(y_true = df$obs, y_pred = df$pred, positive = 1, beta = 2)
      aucpr = PRROC::pr.curve(df$conflict[df$obs == 1], df$conflict[df$obs == 0], curve = F)$auc.integral
      auc = PRROC::roc.curve(df$conflict[df$obs == 1], df$conflict[df$obs == 0], curve = F)$auc
      df$pred = factor(df$pred, levels = c(0,1), labels = c("peace", "conflict"))
      df$obs = factor(df$obs, levels = c(0,1), labels = c("peace", "conflict"))
      cnf = caret::confusionMatrix(df$pred, reference = df$obs, positive = "conflict", mode = "prec_recall")
      kappa = cnf$overall[2]
      accuracy = cnf$overall[1]
      precision = cnf$byClass[5]
      recall = cnf$byClass[6]
      specificity = cnf$byClass[2]
      sensitivity = cnf$byClass[1]
      stats = data.frame(name = c("accuracy","precision","recall","specificity","sensitivity","brier", "f1", "f2", "auc", "aucpr", "kappa", "threshold"),
                         score = c(accuracy, precision, recall, specificity, sensitivity, bs, f1, f2, auc, aucpr, kappa, threshold),#
                         month = l,
                         rep = k)
      stats
    })
    
    fold_results <- do.call(rbind, fold_results)
    results[[l]] = fold_results
  }
  
  results <- do.call(rbind, results)
  results$unit = unit
  results$var = var
  results %>%
    group_by(name, rep) %>%
    summarise(score = mean(score)) %>%
    ungroup() -> month0
  month0$month = 0
  month0$var = var
  month0$unit = unit
  results = rbind(month0, results)
  saveRDS(results, file.path(envrmt$`env_test-results`, paste0("test-results-regression-", unit, "-", var, ".rds")))
}
