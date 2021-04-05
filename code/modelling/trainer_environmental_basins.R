# Training EV predictor CNN-LSTM for basin level
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
ncores = 1

conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))
predictors = readRDS(file.path(envrmt$env_vector, "predictor_cube.rds"))

conflicts %<>%
  filter(unit == "basins", time > as.Date("2000-12-01"))

predictors %<>%
  filter(unit == "basins", time > as.Date("2000-12-01"))

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
# loop through response variables
for (var in vars){
  print(paste0("Starting training for response variable ", var, "."))
  paras = readRDS(file.path(envrmt$env_bayes, paste0(paste("paras", type, unit, sep = "-"), ".rds")))
  # create new training data
  data = split_data(data_pred = predictors, data_conf = conflicts, thres = 0, y_var = var, seed = seed)
  repeats = 10
  rep_results = lapply(1:repeats, function(rep){
    if(!file.exists(file.path(envrmt$`env_test-results`, paste0(paste("repeat", rep, "test", type, unit, var, sep = "-"), ".rds")))){
      # declaration of prediction scope
      dims = dim(data$train$x[[1]])
      len = dims[2]
      min_len = 48
      horizon = 12
      steps = 1
      max_steps = (( len - min_len ) / steps) + 1
      batchsize = dims[1]
      max_batches = dims[1] / batchsize
      if(max_batches %% 1 != 0) max_batches = floor(max_batches) + 1
      
      # train with validation to retrieve thresholds
      metrics = list(tf$metrics$AUC(curve = "PR", name = "aupr"),
                     tf$metrics$AUC(curve = "ROC", name = "auc"),
                     # tfa$metrics$F1Score(num_classes = 2L, name = "f1"),
                     #metrics_f1score(num_classes = 2L, average = "micro", threshold =.2, name = "f1"),
                     metric_fbetascore(num_classes = 2L, beta = 2, name = "f2", average = "micro", threshold = .5),
                     tf$metrics$BinaryAccuracy(name = "accuracy"),
                     tf$metrics$Precision(name = "precision"),
                     tf$metrics$Recall(name = "recall"))
      
      model = compile_model(paras, horizon = horizon, n_predictors = dims[3], metrics = metrics)
      plot_keras_model(model, show_layer_names = TRUE, show_shapes = TRUE, 
                       to_file = file.path(envrmt$env_plots, paste0(paste("model", "plot", type, unit, var, sep = "_"), ".png")))
      
      gen = keras:::as_generator.function(
        batch_generator(x_train=data$train$x, 
                        y_train=data$train$y, 
                        min_len=min_len, 
                        steps=steps, 
                        horizon=horizon,
                        batchsize = batchsize, 
                        return_weights = T,
                        return_window = F)
      )
      
      callbacks = callback_early_stopping(monitor = "val_f2",
                                          mode = "max",
                                          patience = 20,
                                          min_delta = 0.0001,
                                          restore_best_weights = TRUE)
      
      history = model %>% fit_generator(gen, 
                                        steps_per_epoch =  max_steps*max_batches, 
                                        epochs = 200, 
                                        verbose = 1, 
                                        callbacks = callbacks,
                                        validation_data = list(data$val$x, data$val$y))
      
      saveRDS(history, file =  file.path(envrmt$`env_test-results`, paste0(paste("repeat", rep, "history", type, unit, var, sep = "-"),".rds")))
      
      stats = acc_stats(model, 
                        x_test = data$val$x, 
                        y_true =  data$val$y, 
                        ncores = ncores)
      
      # retriain with complete data set
      x_train = data$val$x
      y_train = array(data$val$y, dim = c(dim(data$val$y)[1:2], 1))
      y_train = abind(data$train$y, y_train, along = 2)
      dims = dim(x_train[[1]])
      len = dims[2]
      min_len = 48
      horizon = 12
      steps = 1
      max_steps = (( len - min_len ) / steps) + 1
      batchsize = dims[1]
      max_batches = dims[1] / batchsize
      if(max_batches %% 1 != 0) max_batches = floor(max_batches) + 1
      
      callbacks = callback_early_stopping(monitor = "loss",
                                          mode = "min",
                                          patience = 10,
                                          min_delta = 0.0001,
                                          restore_best_weights = FALSE)
      gen = keras:::as_generator.function(
        batch_generator(x_train=x_train, 
                        y_train=y_train, 
                        min_len=min_len, 
                        steps=steps, 
                        horizon=horizon,
                        batchsize = batchsize, 
                        return_weights = T,
                        return_window = F)
      )
      
      model %>% fit_generator(gen, 
                              steps_per_epoch =  max_steps*max_batches, 
                              epochs = 200, 
                              verbose = 1, 
                              callbacks = callbacks)
      save_model_hdf5(model, filepath = file.path(envrmt$env_models, paste0(paste("repeat", rep, "model", type, unit, var, sep = "-"),".h5")))
      
      # save accuracy statistics
      stats = acc_test(model, 
                       x_test = data$test$x, 
                       y_true = data$test$y, 
                       threshold = stats$score[stats$name == "threshold"], 
                       ncores = ncores)
      stats$unit = unit
      stats$var = var
      stats$rep = rep
      head(stats, 11)
      saveRDS(stats, file = file.path(envrmt$`env_test-results`, paste0(paste("repeat", rep, "test", type, unit, var, sep = "-"), ".rds")))
      
      # save spatial prediction
      pred = predict(model, data$test$x)
      pred = matrix(pred, nrow = dim(pred)[1], ncol = dim(pred)[2])
      for (i in 1:ncol(pred)){
        pred[,i] = if_else(pred[,i]<stats$score[stats$name == "threshold"], 0, 1)
      }
      
      obsv = data$test$y
      pred = as.data.frame(pred)
      obsv = as.data.frame(obsv)
      names(pred) = paste("pred", "m", 1:12, sep = "-")
      names(obsv) = paste("obsv", "m", 1:12, sep = "-")
      pred["pred-sum"] = rowSums(pred)
      obsv["obsv-sum"] = rowSums(obsv)
      attributes = cbind(pred, obsv)
      poly = st_read(file.path(envrmt$env_vector, paste0(unit, "_mask.gpkg")))
      geom = st_geometry(poly)
      poly = cbind(st_drop_geometry(poly), attributes)
      poly$geom = geom
      poly = st_as_sf(poly)
      st_write(poly, file.path(envrmt$`env_test-results`, paste0(paste("repeat", rep, "pred",type,unit,var,sep="-"),".gpkg")), delete_dsn = TRUE)
      stats
    } else {
      stats = readRDS(file.path(envrmt$`env_test-results`, paste0(paste("repeat", rep, "test", type, unit, var, sep = "-"), ".rds")))
      stats
    }
  })
  stats <- do.call(rbind, rep_results)
  saveRDS(stats, file = file.path(envrmt$`env_test-results`, paste0(paste("test", "results", type, unit, var, sep = "-"), ".rds")))
}
