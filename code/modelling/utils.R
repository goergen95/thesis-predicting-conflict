# Helper functions for the masterthesis project
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

split_data_baseline <- function(data_conf, thres, y_var, shuffle = T, seed = 42) {
  
  # prepare conflict data cube
  data_conf %>% 
    filter(type %in% c(as.character(y_var), paste(y_var, c("50", "100", "200"), sep = "_"))) %>%
    as_tibble() %>%
    dplyr::select(-unit) %>%
    mutate(value = if_else(value>thres, as.numeric(1), as.numeric(0)),
           buffer = if_else(str_detect(type, "_"), as.numeric(str_remove(type, pattern=paste0(y_var,"_"))), 0)) %>%
    group_split(buffer) -> data_conf
  
  # Training data
  x_train = lapply(data_conf, function(x){
    x %>%
      filter(time <= as.Date("2016-12-01")) %>% 
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out$mets$value
  })
  
  data_conf[[1]] %>% 
    filter(time <= as.Date("2017-12-01"),
           type == y_var) %>%
    dplyr::select(-buffer) %>%
    as.tbl_cube(dim_names = 1:3) -> y_train
  y_train = y_train$mets$value
  dim(y_train)
  
  # Validation Data
  x_val = lapply(data_conf, function(x){
    x %>%
      filter(time <= as.Date("2017-12-01")) %>% 
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out$mets$value
  })
  
  data_conf[[1]] %>% 
    filter(time >= as.Date("2018-01-01"),
           time <= as.Date("2018-12-01")) %>%
    as.tbl_cube(dim_names = 1:3) -> y_val
  y_val = y_val$mets$value
  # y_val = array(y_val, dim = c(dim(y_val)))
  dim(y_val)
  
  # Test Data
  x_test = lapply(data_conf, function(x){
    x %>%
      filter(time <= as.Date("2018-12-01")) %>% 
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out$mets$value
  })
  
  data_conf[[1]] %>% 
    filter(time >= as.Date("2019-01-01"),
           time <= as.Date("2019-12-01")) %>%
    as.tbl_cube(dim_names = 1:3) -> y_test
  y_test = y_test$mets$value
  dim(y_test)
  
  if(shuffle){
    set.seed(seed)
    index = sample(1:dim(x_train[[1]])[1], dim(x_train[[1]])[1])
    x_train = lapply(x_train, function(x){ x[index,,,drop=FALSE] })
    y_train = y_train[index,,,drop=FALSE]
    x_val = lapply(x_val, function(x){ x[index,,,drop=FALSE] })
    y_val = y_val[index,,,drop=FALSE]
  }
  
  list(train = list(x = x_train, y = y_train), 
       val = list(x = x_val, y = y_val), 
       test = list(x = x_test, y = y_test))
}


split_data <- function(data_pred, 
                       data_conf, 
                       thres, 
                       y_var, 
                       shuffle = T,
                       seed) {
  
  # prepare conflict data cube
  data_conf %<>% 
    filter(type %in% c(as.character(y_var), paste(y_var, c("50", "100", "200"), sep = "_"))) %>%
    as_tibble() %>%
    dplyr::select(-unit) %>%
    mutate(value = if_else(value>thres, as.numeric(1), as.numeric(0)),
           buffer = if_else(str_detect(type, "_"), as.numeric(str_remove(type, pattern=paste0(y_var,"_"))), 0)) %>%
    group_split(buffer)
  
  data_pred %<>% 
    as_tibble() %>%
    mutate(buffer = str_split_fixed(var, "_", 2)[,2],
           buffer = as.numeric(if_else(buffer=="", "0", buffer))) %>%
    group_split(buffer)
  
  merge_train <- function(x, y){
    lapply(1:length(x), function(i){
      out =  abind(x[[i]], y[[i]], along = 3)
      array_names = dimnames(out)
      names(array_names) = c("samples", "time", "features")
      array_names$samples = 1:dim(out)[1]
      array_names$time = 1:dim(out)[2]
      array_names$features = 1:dim(out)[3]
      dimnames(out) = array_names
      out
    })
  }
  
  # Training data
  x_train = lapply(data_pred, function(x){
    x %>%
      dplyr::filter(time <= as.Date("2016-12-01")) %>% 
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out$mets$value
  })
  
  train_conf = lapply(data_conf, function(x){
    x %>% 
      dplyr::filter(time <= as.Date("2016-12-01")) %>%
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out$mets$value
  })
  
  x_train = merge_train(x_train, train_conf)
  str(x_train)
  
  data_conf[[1]] %>% 
    filter(time <= as.Date("2017-12-01"),
           type == y_var) %>%
    dplyr::select(-buffer) %>%
    as.tbl_cube(dim_names = 1:3) -> y_train
  y_train = y_train$mets$value
  dim(y_train)
  
  # clean up
  rm(train_conf)
  gc()
  
  
  # Validation Data
  x_val = lapply(data_pred, function(x){
    x %>%
      filter(time <= as.Date("2017-12-01")) %>% 
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out$mets$value
  })
  
  train_conf = lapply(data_conf, function(x){
    x %>% 
      dplyr::filter(time <= as.Date("2017-12-01")) %>%
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out $mets$value
  })
  
  x_val = merge_train(x_val, train_conf)
  str(x_val)
  
  data_conf[[1]] %>% 
    filter(time >= as.Date("2018-01-01"),
           time <= as.Date("2018-12-01"),
           type == y_var)  %>%
    as.tbl_cube(dim_names = 1:3) -> y_val
  y_val = y_val$mets$value
  dim(y_val)
  
  # clean up
  rm(train_conf)
  gc()
  
  
  # Test Data
  x_test = lapply(data_pred, function(x){
    x %>%
      filter(time <= as.Date("2018-12-01")) %>% 
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out$mets$value
  })
  
  train_conf = lapply(data_conf, function(x){
    x %>% 
      dplyr::filter(time <= as.Date("2018-12-01")) %>%
      as.tbl_cube(dim_names = 1:3) -> x_out
    x_out $mets$value
  })
  
  x_test = merge_train(x_test, train_conf)
  str(x_test)
  
  data_conf[[1]] %>% 
    filter(time >= as.Date("2019-01-01"),
           time <= as.Date("2019-12-01"),
           type == y_var)  %>%
    as.tbl_cube(dim_names = 1:3) -> y_test
  y_test = y_test$mets$value
  dim(y_test)
  
  # clean up
  rm(train_conf)
  gc()
  
  
  if(shuffle){
    set.seed(seed)
    index = sample(1:dim(x_train[[1]])[1], dim(x_train[[1]])[1])
    x_train = lapply(x_train, function(x){ x[index,,,drop=FALSE] })
    y_train = y_train[index,,,drop=FALSE]
    x_val = lapply(x_val, function(x){ x[index,,,drop=FALSE] })
    y_val = y_val[index,,,drop=FALSE]
  }
  
  list(train = list(x = x_train, y_train = y_train), 
       val = list(x = x_val, y = y_val), 
       test = list(x = x_test, y = y_test))
}

batch_generator <- function(x_train, 
                            y_train, 
                            min_len = 48, 
                            steps = 1, 
                            horizon = 12,
                            batchsize = 10,
                            return_weights = T,
                            return_window = F){
  
  dims = dim(x_train[[1]])
  len = dims[2]
  w_steps = 1/len
  n_samples = dims[1]
  max_batches = n_samples / batchsize
  if(max_batches %% 1 != 0) max_batches = floor(max_batches) + 1
  # max_batches = ((( len - min_len ) / steps) + 1) * n_batches
  max_timesteps =  ((( len - min_len ) / steps) + 1)
  
  current_batch <- 1
  current_timestep <- 1
  
  function() {
    
    if(return_window){
      start_train = (steps * (current_timestep - 1)) + 1
    } else {
      start_train = 1 
    }
    end_train = min_len + (steps * (current_timestep - 1))
    start_test = end_train + 1
    
    x_return = lapply(x_train, function(x){ 
      x[1:dims[1], start_train:end_train, 1:dims[3], drop = FALSE]
    })
    y_return = y_train[1:dims[1], start_test:(end_train+horizon), 1, drop = FALSE]
    
    if(current_batch*batchsize < dims[1]){
      x_return = lapply(x_return, function(x) {
        x[(current_batch*batchsize-batchsize+1):(current_batch*batchsize), 1:(end_train-start_train+1), 1:dims[3], drop = FALSE]
      })
      y_return = y_return[(current_batch*batchsize-batchsize+1):(current_batch*batchsize), 1:horizon, 1 , drop=FALSE]
    } else {
      x_return = lapply(x_return, function(x){
        x[(current_batch*batchsize-batchsize+1):dims[1], 1:(end_train-start_train+1), 1:dims[3], drop = FALSE]
      })
      y_return = y_return[(current_batch*batchsize-batchsize+1):dims[1], 1:horizon, 1 , drop=FALSE]
    }
    
    current_batch <<- current_batch + 1 
    if(current_batch > max_batches){
      current_batch <<- 1
      current_timestep <<- current_timestep + 1 
    }
    if(current_timestep > max_timesteps){
      current_timestep <<- 1
    }
    # print(current_step)
    w_return = array(rep(w_steps * dim(x_return[[1]])[2], dim(x_return[[1]])[1]), dim = c(dim(x_return[[1]])[1], 1))
    w_return = lapply(1:length(x_return), function(i) w_return)
    if(return_weights){
      return(list(x_return, y_return, w_return))
    } else {
      return(list(x_return, y_return))
    }
  }
}


MyHyperModel <- reticulate::PyClass(
  "HyperModel",
  
  inherit = kerastuneR::HyperModel_class(),
  
  list(
    
    `__init__` = function(self, 
                          input_shape, 
                          num_classes, 
                          metrics, 
                          lstm_layers) {
      
      self$input_shape = input_shape
      self$num_classes = num_classes
      self$metrics = metrics
      self$lstm_layers = lstm_layers
      NULL
      # self = list()
      # self$num_classes = 12
      # self$input_shape = list(NULL, 1)
      # self$metrics = metric_fbetascore(2, beta = 2, threshold = .5)
      # self$lstm_layers = 3L
    },
    build = function(self, hp){
      
      
      inputUnit <- layer_input(shape = self$input_shape, name = "input_unit")
      inputB50 <- layer_input(shape =  self$input_shape, name = "input_B50")
      inputB100 <- layer_input(shape = self$input_shape, name = "input_B100")
      inputB200 <- layer_input(shape = self$input_shape, name = "input_B200")
      inputs = c("inputUnit", 
                 "inputB50", 
                 "inputB100", 
                 "inputB200")
      
      # hp = HyperParameters()
   
      
      for(input in inputs){
        
        hp$Int(name = paste(input, "lstm_layers", sep = "_"), 
               min_value = 1L, 
               max_value = self$lstm_layers, 
               default=1L)
        
        hp$Boolean(name = paste(input, "double_cnn", sep = "_"),
                   default = FALSE)
        
        hp$Int(name = paste(input, "cnn_filters", sep = "_"),
               min_value = 12, 
               max_value = 128)
        hp$Int(name = paste(input, "cnn_kernel", sep = "_"),
               min_value = 3L, 
               max_value = 24L)
        hp$Choice(name = paste(input, "cnn_activation", sep = "_"),
                  default = "sigmoid",
                  values = c(
                    "sigmoid", 
                    "hard_sigmoid", 
                    "softmax",
                    "softplus",
                    "softsign"))
       hp$Choice(paste(input, "cnn_pooling", sep= "_"),
                 values = c("max", "average"),
                  default = "max")
        hp$Int(name = paste(input, "pool_size", sep = "_"),
               min_value = 3L,
               max_value = 24L,
               default = 3L)
        hp$Choice(paste(input, "global_pooling", sep = "_"),
                  values = c("max", "average"),
                  default = "max")
      }
      
      for (lstm in 1:self$lstm_layers){
        for (input in inputs){
          hp$Int(name = paste(input, 'lstm_neurons', lstm, sep = "_"), 
                 min_value = 12L, 
                 max_value = 128L,
                 default=64L,
                 parent_name = paste(input, "lstm_layers", sep = "_"),
                 parent_values = c(lstm:self$lstm_layers))
          hp$Float(name = paste(input, 'lstm_dropout', lstm, sep = "_"), 
                   min_value = 0, 
                   max_value = .5, 
                   default = .25,
                   step = 0.01,
                   parent_name = paste(input, "lstm_layers", sep = "_"),
                   parent_values = c(lstm:self$lstm_layers))
        }
      }
      
      branches <- lapply(inputs, 
                         function(input){
                           
                           if(input == "inputUnit"){
                             y = inputUnit
                           }
                           if(input == "inputB50"){
                             y = inputB50
                           }
                           if(input == "inputB100"){
                             y = inputB100
                           }
                           if(input == "inputB200"){
                             y = inputB200
                           }
                           
                           x <- y %>%
                             layer_conv_1d(filters = hp$get(paste(input, "cnn_filters", sep = "_")), 
                                           kernel_size = hp$get(paste(input, "cnn_kernel", sep = "_")), 
                                           padding = "same",
                                           data_format = "channels_last",
                                           input_shape = self$input_shape,
                                           name = paste(input, "cnn_1", sep = "_"),
                                           activation = hp$get(paste(input, "cnn_activation", sep = "_")))
                           if(hp$get(paste(input, "double_cnn", sep = "_"))){
                             x %<>% layer_conv_1d(filters = hp$get(paste(input, "cnn_filters", sep = "_")), 
                                                  kernel_size = hp$get(paste(input, "cnn_kernel", sep = "_")), 
                                                  padding = "same",
                                                  data_format = "channels_last",
                                                  name = paste(input, "cnn",2, sep = "_"),
                                                  activation = hp$get(paste(input, "cnn_activation", sep = "_")))
                           }
                           # add pooling layer
                           
                           if(hp$get(paste(input, "cnn_pooling", sep = "_")) == "max") x %<>% layer_max_pooling_1d(pool_size = hp$get(paste(input, "pool_size", sep = "_")), padding = "same")
                           if(hp$get(paste(input, "cnn_pooling", sep = "_")) == "average") x %<>% layer_average_pooling_1d(pool_size = hp$get(paste(input, "pool_size", sep = "_")), padding = "same")
                           if(hp$get(paste(input, "global_pooling", sep = "_")) == "max") x %<>% layer_global_max_pooling_1d()
                           if(hp$get(paste(input, "global_pooling", sep = "_")) == "average") x %<>% layer_global_average_pooling_1d()
                           # repeat vector for n outcome timesteps
                           x %<>%
                             layer_repeat_vector(self$num_classes)
                           
                           # add LSTM layer
                           for (lstm in 1:hp$get(paste(input, "lstm_layers", sep = "_"))){
                             x %<>%
                               layer_lstm(units = hp$get(paste(input, "lstm", "neurons", lstm, sep = "_")),
                                          name = paste(input, "lstm", lstm, sep = "_"),
                                          activation = "tanh", # needed for cudnn
                                          recurrent_activation = "sigmoid", # needed for cudnn
                                          recurrent_dropout = 0, # needed for cuddnn
                                          unroll = FALSE, # needed for cudnn
                                          use_bias = TRUE, # needed fo cudnn
                                          return_sequences = TRUE, # last layer should return its output
                                          stateful = FALSE) %>%
                               layer_dropout(rate =  hp$get(paste(input, 'lstm_dropout', lstm, sep = "_")))
                           }
                           x
                         })
      
      
      hp$Int(name = paste('dense_units'), 
             min_value = 12L, 
             max_value = 128L,
             default=64L)
      hp$Choice("dense_activation",
                default = "relu",
                values = c(
                  "sigmoid", 
                  "hard_sigmoid", 
                  "softmax",
                  "softplus",
                  "softsign",
                  "relu",
                  "elu",
                  "selu",
                  "tanh"))
      
      hp$Float(name = "gamma", 
               min_value = 0, 
               max_value = 10, 
               default = 2,
               step = 0.125)
      hp$Float(name = "alpha",
               min_value = 0,
               max_value = 1,
               default = .25,
               step = 0.001)
      
      gamma = hp$get("gamma")
      alpha = hp$get("alpha")
      loss_func = tfa$losses$SigmoidFocalCrossEntropy(alpha = alpha, gamma = gamma)
      
      hp$Float("pi",
               min_value = 0,
               max_value = 1,
               default = .001,
               step = 0.001)
      
      bias_init = tf$constant_initializer(hp$get("pi"))
      
      names(branches) = c("unit", "b50", "b100", "b200")
      merge <- layer_concatenate(c(branches[[1]], branches[[2]], branches[[3]], branches[[4]]))
      merge %<>%
        layer_dense(units = hp$get('dense_units'), activation = hp$get('dense_activation')) %>% 
        layer_dense(units = hp$get('dense_units'), activation = hp$get('dense_activation')) %>% 
        layer_dense(units = hp$get('dense_units'), activation = hp$get('dense_activation')) %>% 
        keras::time_distributed(
          layer_dense(units = 1, 
                      name = "output",
                      activation = hp$Choice(name = "out_activation", 
                                             default = "sigmoid",
                                             values = c("sigmoid", 
                                                        "hard_sigmoid", 
                                                        "softmax")),
                      bias_initializer =  bias_init)
        )
      
      model <- keras_model(inputs = c(inputUnit,inputB50, inputB100, inputB200), merge)
      
      optimizer = hp$Choice(name = "optimizer", 
                            default = "rmsprop",
                            values = c("adam", 
                                       "rmsprop", 
                                       "adadelta", 
                                       "adagrad", 
                                       "adamax", 
                                       "sgd"))
      lr = hp$Float(name = "lr", 
                    min_value = 0.00001, 
                    max_value = 1, 
                    default = 0.01,
                    step = 0.000005)
      
      if(optimizer == "adam"){
        optimizer = optimizer_adam(lr = lr)
      } else if(optimizer == "rmsprop"){
        optimizer = optimizer_rmsprop(lr = lr)
      } else if (optimizer == "adadelta"){
        optimizer = optimizer_adadelta(lr = lr)
      } else if (optimizer == "adagrad"){
        optimizer = optimizer_adagrad(lr = lr)
      } else if (optimizer == "adamax"){
        optimizer = optimizer_adamax(lr = lr)
      } else if (optimizer == "sgd"){
        optimizer = optimizer_sgd(lr = lr)
      }
      
      model %<>% compile(optimizer = optimizer, 
                         loss = loss_func,
                         metrics=self$metrics)
      return(model)
    }
  )
)


acc_stats <- function(model, x_test, y_true, ncores){
  
  # global analysis
  y_pred = c(predict(model, x_test, type = "prob"))
  # y_pred = matrix(y_pred, ncol = dim(y_pred)[2])
  y_true = c(y_true)
  
  df = data.frame(conflict = y_pred, 
                  peace = 1 - y_pred, 
                  obs = y_true,
                  month = rep(1:12, length(y_true)))
  
  thresholds = seq(.001,1, 0.001)
  
  results = mclapply(thresholds, function(thres){
    tmp = df
    tmp$pred = if_else(tmp$conflict<thres, 0, 1)
    if(length(unique(tmp$pred)) == 1) return(data.frame(thres = as.numeric(thres), score = NA))
    score = MLmetrics::FBeta_Score(y_true = tmp$obs, y_pred = tmp$pred, positive = 1, beta = 2)
    #score = MLmetrics::F1_Score(y_true=tmp$obs, y_pred = tmp$pred, positive = 1)
    data.frame(thres = as.numeric(thres), score = as.numeric(score))
  }, mc.cores = ncores)
  
  # select best threshold
  results = do.call(rbind, results)
  results = results[which(results$score == max(results$score, na.rm = TRUE)),]
  if(nrow(results)>1){
    results = results[sample(nrow(results), 1),]
  }
  threshold = results$thres
  
  monthly <- mclapply(0:12, function(i){  
    
    if(i == 0 ){
      tmp = df
    }else{
      tmp = filter(df, month == i)
    }
    
    tmp$pred = if_else(tmp$conflict<threshold[1], 0, 1)
    bs = mean((tmp$conflict-tmp$obs)^2)
    f1 = MLmetrics::F1_Score(y_true = tmp$obs, y_pred = tmp$pred, positive = 1)
    f2 = MLmetrics::FBeta_Score(y_true = tmp$obs, y_pred = tmp$pred, positive = 1, beta = 2)
    aucpr = PRROC::pr.curve(tmp$conflict[tmp$obs == 1], tmp$conflict[tmp$obs == 0], curve = F)$auc.integral
    auc = PRROC::roc.curve(tmp$conflict[tmp$obs == 1], tmp$conflict[tmp$obs == 0], curve = F)$auc
    tmp$pred = factor(tmp$pred, levels = c(0,1), labels = c("peace", "conflict"))
    tmp$obs = factor(tmp$obs, levels = c(0,1), labels = c("peace", "conflict"))
    cnf = caret::confusionMatrix(tmp$pred, reference = tmp$obs, positive = "conflict", mode = "prec_recall")
    kappa = cnf$overall[2]
    accuracy = cnf$overall[1]
    precision = cnf$byClass[5]
    recall = cnf$byClass[6]
    specificity = cnf$byClass[2]
    sensitivity = cnf$byClass[1]
    tibble(name = c("accuracy", 
                    "precision",
                    "recall",
                    "specificity",
                    "sensitivity",
                    "brier",
                    "f1", 
                    "f2", 
                    "auc", 
                    "aucpr", 
                    "kappa"), 
           score = c(accuracy, 
                     precision, 
                     recall, 
                     specificity, 
                     sensitivity, 
                     bs, 
                     f1, 
                     f2, 
                     auc, 
                     aucpr, 
                     kappa),
           month = i)
  }, mc.cores = ncores)
  
  monthly = do.call(rbind, monthly)
  thres = tibble(name = "threshold",
                     score = threshold,
                 month = 0)
  
  rbind(thres, monthly)
  
}

acc_test <- function(model, x_test, y_true, threshold, ncores){
  
  # global analysis
  y_pred = c(predict(model, x_test, type = "prob"))
  # y_pred = matrix(y_pred, ncol = dim(y_pred)[2])
  y_true = c(y_true)
  
  df = data.frame(conflict = y_pred, 
                  peace = 1 - y_pred, 
                  obs = y_true,
                  month = rep(1:12, length(y_true)))
  
  monthly <- mclapply(0:12, function(i){  
    
    if(i == 0 ){
      tmp = df
    }else{
      tmp = filter(df, month == i)
    }
    
    tmp$pred = if_else(tmp$conflict<threshold, 0, 1)
    bs = mean((tmp$conflict-tmp$obs)^2)
    f1 = MLmetrics::F1_Score(y_true = tmp$obs, y_pred = tmp$pred, positive = 1)
    f2 = MLmetrics::FBeta_Score(y_true = tmp$obs, y_pred = tmp$pred, positive = 1, beta = 2)
    aucpr = PRROC::pr.curve(tmp$conflict[tmp$obs == 1], tmp$conflict[tmp$obs == 0], curve = F)$auc.integral
    auc = PRROC::roc.curve(tmp$conflict[tmp$obs == 1], tmp$conflict[tmp$obs == 0], curve = F)$auc
    tmp$pred = factor(tmp$pred, levels = c(0,1), labels = c("peace", "conflict"))
    tmp$obs = factor(tmp$obs, levels = c(0,1), labels = c("peace", "conflict"))
    cnf = caret::confusionMatrix(tmp$pred, reference = tmp$obs, positive = "conflict", mode = "prec_recall")
    kappa = cnf$overall[2]
    accuracy = cnf$overall[1]
    precision = cnf$byClass[5]
    recall = cnf$byClass[6]
    specificity = cnf$byClass[2]
    sensitivity = cnf$byClass[1]
    tibble(name = c("accuracy", "precision","recall","specificity","sensitivity","brier", "f1", "f2", "auc", "aucpr", "kappa"), 
           score = c(accuracy, precision, recall, specificity, sensitivity, bs, f1, f2, auc, aucpr, kappa),
           month = i)
  }, mc.cores = ncores)
  
  monthly = do.call(rbind, monthly)
  thres = tibble(name = "threshold",
                 score = threshold,
                 month = 0)
  
  rbind(thres, monthly)
}

compile_model <- function(paras, horizon, n_predictors, metrics){
  
  
  inputUnit <- layer_input(shape = list(NULL, n_predictors), name = "input_unit")
  inputB50 <- layer_input(shape = list(NULL, n_predictors), name = "input_B50")
  inputB100 <- layer_input(shape = list(NULL, n_predictors), name = "input_B100")
  inputB200 <- layer_input(shape = list(NULL, n_predictors), name = "input_B200")
  inputs <- c("inputUnit", "inputB50", "inputB100", "inputB200")
  
  # build branches structure
  # build conv block
  branches <- lapply(inputs, function(input){
    
    if(input == "inputUnit"){
      y = inputUnit
    }
    if(input == "inputB50"){
      y = inputB50
    }
    if(input == "inputB100"){
      y = inputB100
    }
    if(input == "inputB200"){
      y = inputB200
    }
    
    x <- y %>%
      layer_conv_1d(filters = paras[paste(input, "cnn_filters", sep = "_")][[1]], 
                    kernel_size = paras[paste(input, "cnn_kernel", sep = "_")][[1]], 
                    padding = "same",
                    data_format = "channels_last",
                    name = paste(input, "cnn", "block", 1,sep = "_"),
                    activation = paras[paste(input, "cnn_activation", sep = "_")][[1]])
    
    if(unlist(paras[paste0(input, "_double_cnn")])){
      x %<>%
        layer_conv_1d(filters = paras[paste(input, "cnn_filters", sep = "_")][[1]], 
                      kernel_size = paras[paste(input, "cnn_kernel", sep = "_")][[1]], 
                      padding = "same",
                      data_format = "channels_last",
                      name = paste(input, "cnn", "block", 2, sep = "_"),
                      activation = paras[paste(input, "cnn_activation", sep = "_")][[1]])
    }
    # add pooling layer
   # if(paras[paste(input, "cnn_pooling", sep = "_")] == "max") x %<>% layer_max_pooling_1d(pool_size = paras[paste(input, "pool_size", sep = "_")], padding = "same")
  #  if(paras[paste(input, "cnn_pooling", sep = "_")] == "average") x %<>% layer_average_pooling_1d(pool_size = paras[paste(input, "pool_size", sep = "_")], padding = "same")
    if(paras[paste(input, "global_pooling", sep = "_")] == "max") x %<>% layer_global_max_pooling_1d()
    if(paras[paste(input, "global_pooling", sep = "_")] == "average") x %<>% layer_global_average_pooling_1d()
    # repeat vector for n outcome timesteps
    x %<>%
      layer_repeat_vector(horizon)
    
    neurons = unlist(paras[grep("neurons", names(paras))])
    neurons = neurons[grep(input, names(neurons))]
    dropout = unlist(paras[grep("dropout", names(paras))])
    dropout = dropout[grep(input, names(dropout))]
    
    # add LSTM layer
    for (i in 1:unlist(paras[paste0(input, "_lstm_layers")])){
      x %<>%
        layer_lstm(units = as.integer(neurons[i]),
                   activation = "tanh",
                   name = paste0(input, "_lstm_", i),
                   recurrent_activation = "sigmoid",
                   recurrent_dropout = 0,
                   unroll = FALSE,
                   use_bias = T,
                   return_sequences = TRUE,
                   stateful = F) %>%
        layer_dropout(rate = as.numeric(dropout[i]), name = paste0(input, "_drop_", i))
    }
    x
  })
  
  names(branches) = c("unit", "b50", "b100", "b200")
  merge <- layer_concatenate(c(branches[[1]], branches[[2]], branches[[3]], branches[[4]]))
  
  merge %<>%
    layer_dense(units = paras$dense_units, activation = "relu") %>% 
    layer_dense(units = paras$dense_units, activation = "relu") %>% 
    layer_dense(units = paras$dense_units, activation = "relu") %>% 
    keras::time_distributed(
      layer_dense(units = 1, 
                  activation = paras$out_activation,
                  bias_initializer =  tf$constant_initializer(paras$pi),
                  name = "output")
    )
  
  model <- keras_model(inputs = c(inputUnit,inputB50, inputB100, inputB200), merge)
  
  if(paras$optimizer == "adam"){
    optimizer = optimizer_adam(lr = paras$lr)
  } else if(paras$optimizer == "rmsprop"){
    optimizer = optimizer_rmsprop(lr = paras$lr)
  } else if (paras$optimizer == "adadelta"){
    optimizer = optimizer_adadelta(lr = paras$lr)
  } else if (paras$optimizer == "adagrad"){
    optimizer = optimizer_adagrad(lr = paras$lr)
  } else if (paras$optimizer == "adamax"){
    optimizer = optimizer_adamax(lr = paras$lr)
  } else if (paras$optimizer == "sgd") {
    optimizer = optimizer_sgd(lr = paras$lr)
  }
  
  
  loss = tfa$losses$SigmoidFocalCrossEntropy(alpha = paras$alpha, gamma = paras$gamma)
  
  model %>%
    compile(
      loss = loss,
      optimizer = optimizer,
      metrics = metrics
    )
  model
}

