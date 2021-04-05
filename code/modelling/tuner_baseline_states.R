# Bayesian Optimization of CH baseline CNN-LSTM for admin level
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

# split data to train, val and test
data = split_data_baseline(conflicts, thres = 0, y_var = "all", seed = seed)
pos = sum(data$test$y_test == 1 )
neg = sum(data$test$y_test == 0 )
print(pos / (pos + neg) * 100)
# declaration of prediction scope
dims = dim(data$train$x[[1]])
len = dims[2]
min_len = 48
horizon = 12
steps = 4
max_steps = (( len - min_len ) / steps) + 1
batchsize = dims[1]
max_batches = dims[1] / batchsize
if(max_batches %% 1 != 0) max_batches = floor(max_batches) + 1
# declare a training data generator which is able to be used in tuner
# workaround found at https://github.com/rstudio/keras/issues/1090
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

metric = metric_fbetascore(num_classes = 2, beta = 2, name = "f2", average = "micro", threshold = .5)
# Declare a model with hyperparameter search space
mymodel = MyHyperModel(input_shape = list(NULL,dims[3]), 
                       num_classes = horizon, 
                       metrics =  metric,
                       lstm_layers = 3L)
# self = list()
# self$input_shape = list(NULL, dims[3])
# self$num_classes = horizon
# self$metrics = metric_fbetascore(num_classes = 1, beta = 2, name = "f2", average = "micro", threshold = .17)
# self$lstm_layers = 3L
# self$max_window = 12L
# model %>% fit_generator(gen, validation_data = list(data$val$x, data$val$y), epochs = 10, steps_per_epoch = max_steps*max_batches)



# define bayesian tuner
tuner = BayesianOptimization(mymodel,
                             objective = kerastuneR::Objective("val_f2", direction = "max"),
                             max_trials = 200,
                             num_initial_points = 100,
                             directory = envrmt$env_bayes,
                             project_name = paste("bayesian", type, unit, sep = "-"),
                             overwrite = F,
                             seed = seed)
# tuner %>% search_summary()
# declaration of early stopping to prevent overfitting
callbacks = callback_early_stopping(monitor = "val_f2",
                                    mode = "max",
                                    patience = 10,
                                    min_delta = 0.001,
                                    restore_best_weights = TRUE)
# call to tune the model
# x_val = data$val$x_val
# x_val = x_val[1:dim(x_val)[1], (dim(x_val)[2]-min_len+1):dim(x_val)[2], 1:dim(x_val)[3]]
# y_val = data$val$y_val

tuner %>% fit_tuner(gen,
                    validation_data = list(data$val$x, data$val$y),
                    epochs = 200,
                    batch_size = batchsize,
                    shuffle = F,
                    callbacks = list(callbacks), verbose = 1,
                    steps_per_epoch = max_steps*max_batches)

paras = paras = tuner$get_best_hyperparameters()[[1]]$values
saveRDS(paras, file.path(envrmt$env_bayes, paste0(paste("paras", type, unit, sep = "-"),".rds")))
