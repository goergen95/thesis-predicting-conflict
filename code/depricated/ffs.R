
# ffs function taken and adpted from Hanna Meyer's CAST package
# https://github.com/HannaMeyer/CAST/blob/master/R/ffs.R

ffs <- function (response,
                 predictors,
                 y_var,
                 metric = "f2",
                 maximize = TRUE,
                 minVar = 2,
                 seed = 42,
                 paras,
                 verbose=TRUE,
                 outdir = ".",
                 pattern = "ffs_results",
                 horizon,
                 steps, 
                 batchsize,
                 min_len,
                 patience,
                 max_epochs,
                 shuffle,
                 thres, 
                 selectedvars = NULL){
  
  n <- sum(str_detect(response$dims$type, y_var))
  vars <- response$dims$type[str_detect(response$dims$type, y_var)]
  if(!is.null(predictors)){ 
    n <- length(predictors$dims$var) + n
    vars <- c(predictors$dims$var, vars)
  }
  
  
  if(!is.null(selectedvars)){
    vars_org <- vars
    vars <- vars_org[-which(vars_org %in% selectedvars)]
    n = length(vars)
    minVar = length(selectedvars)
    vars = vars_org
    
  }
  
  acc <- 0
  isBetter <- function (actmodelperf,bestmodelperf,
                        maximization=FALSE){
    result <- ifelse (!maximization, actmodelperf < bestmodelperf,
                      actmodelperf > bestmodelperf)
    return(result)
  }
  
  if(is.null(selectedvars)){
    perf_all <- data.frame(matrix(ncol=length(vars)+2,
                                  nrow=choose(n, minVar)+(n-minVar)*(n-minVar+1)/2))
    names(perf_all) <- c(paste0("var",1:length(vars)),metric,"nvar")
    
    #### chose initial best model from all combinations of two variables
    minGrid <- t(data.frame(combn(vars,minVar)))
    for (i in 1:nrow(minGrid)){
      if (verbose){
        print(paste0("model using ",paste0(minGrid[i,],collapse=","), " will be trained now..." ))
      }
      set.seed(seed)
      if(is.null(predictors)){
        data = split_data_baseline(response, 
                                   thres = thres, 
                                   y_var = y_var, 
                                   x_vars = minGrid[i,], 
                                   shuffle = shuffle,
                                   seed = seed)
      } else {
        data = split_data(predictors, 
                          response, 
                          thres = thres, 
                          y_var = y_var, 
                          x_vars = minGrid[i,], 
                          shuffle = shuffle,
                          seed = seed)
      } 
      # calculate training steps
      dims = dim(data$train$x_train)
      len = dims[2]
      max_steps = (( len - min_len ) / steps) + 1
      max_batches = dims[1] / batchsize
      if(max_batches %% 1 != 0) max_batches = floor(max_batches) + 1
      
      # chose metric
      metrics = list(aupr = tf$metrics$AUC(curve = "PR", name = "aupr"),
                     auc = tf$metrics$AUC(curve = "ROC", name = "auc"),
                     f2 = metric_fbetascore(num_classes = 2L, beta = 2, name = "f2", average = "micro", threshold = .2),
                     accuracy = tf$metrics$BinaryAccuracy(name = "accuracy"),
                     precision = tf$metrics$Precision(name = "precision"),
                     recall = tf$metrics$Recall(name = "recall"))
      # compile model
      model = compile_model(paras, 
                            horizon = horizon, 
                            n_predictors = dim(data$train$x_train)[3], 
                            metrics = metrics[metric][[1]])
      # declare generator
      gen = keras:::as_generator.function(
        batch_generator(x_train=data$train$x_train, 
                        y_train=data$train$y_train, 
                        min_len=min_len, 
                        steps=steps, 
                        horizon=horizon,
                        batchsize = batchsize, 
                        return_weights = T,
                        return_window = F)
      )
      # declare callbacks
      callbacks = callback_early_stopping(monitor = paste0("val_", metric),
                                          mode = if_else(maximize, "max", "min"),
                                          patience = patience,
                                          min_delta = 0.001,
                                          restore_best_weights = TRUE)
      
      history = model %>% fit_generator(gen, 
                                        steps_per_epoch =  max_steps*max_batches, 
                                        epochs = max_epochs, 
                                        verbose = 0, 
                                        callbacks = callbacks,
                                        validation_data = list(data$val$x_val, data$val$y_val))
      
      ### compare the model with the currently best model
      if(maximize){
        actmodelperf <- max(history$metrics[paste0("val_", metric)][[1]], na.rm=T)
      } else {
        actmodelperf <- min(history$metrics[paste0("val_", metric)][[1]], na.rm=T)
      }
      
      if (i == 1){
        bestmodelperf <- actmodelperf
        bestvars =  as.vector(minGrid[i,])
      } else{
        if (isBetter(actmodelperf,bestmodelperf,maximization=maximize)){
          bestmodelperf <- actmodelperf
          bestvars =  as.vector(minGrid[i,])
        }
      }
      acc <- acc+1
      
      variablenames <- minGrid[i,]
      perf_all[acc,1:length(variablenames)] <- variablenames
      perf_all[acc,(length(vars)+1):ncol(perf_all)] <- c(actmodelperf,length(variablenames))
      saveRDS(perf_all, file = file.path(outdir, paste0(pattern, "-perf_all-minGrid.rds")))
      if(verbose){
        print(paste0("maximum number of models that still need to be trained: ",
                     round(choose(n, minVar)+(n-minVar)*(n-minVar+1)/2-acc,0)))
      }
    }
  } else {
    perf_all <- data.frame(matrix(ncol=length(vars_org)+2,
                                  nrow=choose(n, 2)))
    names(perf_all) <- c(paste0("var",1:length(vars_org)),metric,"nvar")
    
    set.seed(seed)
    if(is.null(predictors)){
      data = split_data_baseline(response, 
                                 thres = thres, 
                                 y_var = y_var, 
                                 x_vars = selectedvars, 
                                 shuffle = shuffle,
                                 seed = seed)
    } else {
      data = split_data(predictors, 
                        response, 
                        thres = thres, 
                        y_var = y_var, 
                        x_vars = selectedvars, 
                        shuffle = shuffle,
                        seed = seed)
    } 
    # calculate training steps
    dims = dim(data$train$x_train)
    len = dims[2]
    max_steps = (( len - min_len ) / steps) + 1
    max_batches = dims[1] / batchsize
    if(max_batches %% 1 != 0) max_batches = floor(max_batches) + 1
    
    # chose metric
    metrics = list(aupr = tf$metrics$AUC(curve = "PR", name = "aupr"),
                   auc = tf$metrics$AUC(curve = "ROC", name = "auc"),
                   f2 = metric_fbetascore(num_classes = 2L, beta = 2, name = "f2", average = "micro", threshold = .2),
                   accuracy = tf$metrics$BinaryAccuracy(name = "accuracy"),
                   precision = tf$metrics$Precision(name = "precision"),
                   recall = tf$metrics$Recall(name = "recall"))
    # compile model
    model = compile_model(paras, 
                          horizon = horizon, 
                          n_predictors = dim(data$train$x_train)[3], 
                          metrics = metrics[metric][[1]])
    # declare generator
    gen = keras:::as_generator.function(
      batch_generator(x_train=data$train$x_train, 
                      y_train=data$train$y_train, 
                      min_len=min_len, 
                      steps=steps, 
                      horizon=horizon,
                      batchsize = batchsize, 
                      return_weights = T,
                      return_window = F)
    )
    # declare callbacks
    callbacks = callback_early_stopping(monitor = paste0("val_", metric),
                                        mode = if_else(maximize, "max", "min"),
                                        patience = patience,
                                        min_delta = 0.001,
                                        restore_best_weights = TRUE)
    
    history = model %>% fit_generator(gen, 
                                      steps_per_epoch =  max_steps*max_batches, 
                                      epochs = max_epochs, 
                                      verbose = 0 , 
                                      callbacks = callbacks,
                                      validation_data = list(data$val$x_val, data$val$y_val))
    
    ### compare the model with the currently best model
    if(maximize){
      actmodelperf <- max(history$metrics[paste0("val_", metric)][[1]], na.rm=T)
    } else {
      actmodelperf <- min(history$metrics[paste0("val_", metric)][[1]], na.rm=T)
    }
    
    bestmodelperf <- actmodelperf
    bestvars =  selectedvars    
    acc <- acc+1
    
    perf_all[acc,1:length(bestvars)] <- bestvars
    perf_all[acc,(length(vars)+1):ncol(perf_all)] <- c(actmodelperf,length(selectedvars))
    saveRDS(perf_all, file = file.path(outdir, paste0(pattern, "-perf_all-minGrid.rds")))
    if(verbose){
      print(paste0("maximum number of models that still need to be trained: ",
                   round(choose(n, 2)-acc)))
    }
  }
  #### increase the number of predictors by one (try all combinations)
  #and test if model performance increases
  selectedvars <- bestvars
  selectedvars_perf <- bestmodelperf
  result = list()
  if(verbose){
    print(paste0(paste0("vars selected: ",paste(selectedvars, collapse = ',')),
                 " with ",metric," ",round(selectedvars_perf,3)))
  }
  
  for (k in 1:(length(vars)-minVar)){
    if(k == 1){
      startvars <- bestvars
      nextvars <- vars[-which(vars%in%startvars)]
    } else {
      startvars <- selectedvars
      nextvars = vars[-which(vars%in%startvars)]
    }
    if (length(startvars)<(k+(minVar-1))){
      message(paste0("Note: No increase in performance found using more than ",
                     length(startvars), " variables"))
      result$selectedvars <- selectedvars
      result$selectedvars_perf <- selectedvars_perf
      result$perf_all <- perf_all
      result$perf_all <- result$perf_all[!apply(is.na(result$perf_all), 1, all),]
      result$perf_all <- result$perf_all[colSums(!is.na(result$perf_all)) > 0]
      result$minVar <- minVar
      result$type <- "ffs"
      saveRDS(result, file = file.path(outdir, paste0(pattern, "-result.rds")))
      return(result)
      break()
    }
    for (i in 1:length(nextvars)){
      if(verbose){
        print(paste0("model using additional variable ",nextvars[i], " will be trained now..." ))
      }
      set.seed(seed)
      
      if(is.null(predictors)){
        data = split_data_baseline(response, 
                                   thres = thres, 
                                   y_var = y_var, 
                                   x_vars = c(startvars,nextvars[i]), 
                                   shuffle = shuffle,
                                   seed = seed)
      } else {
        data = split_data(predictors, 
                          response, 
                          thres = thres, 
                          y_var = y_var, 
                          x_vars = c(startvars,nextvars[i]), 
                          shuffle = shuffle,
                          seed = seed)
      } 
      
      # calculate training steps
      dims = dim(data$train$x_train)
      len = dims[2]
      max_steps = (( len - min_len ) / steps) + 1
      max_batches = dims[1] / batchsize
      if(max_batches %% 1 != 0) max_batches = floor(max_batches) + 1
      
      # chose metric
      metrics = list(aupr = tf$metrics$AUC(curve = "PR", name = "aupr"),
                     auc = tf$metrics$AUC(curve = "ROC", name = "auc"),
                     f2 = metric_fbetascore(num_classes = 2L, beta = 2, name = "f2", average = "micro", threshold = .2),
                     accuracy = tf$metrics$BinaryAccuracy(name = "accuracy"),
                     precision = tf$metrics$Precision(name = "precision"),
                     recall = tf$metrics$Recall(name = "recall"))
      # compile model
      model = compile_model(paras, 
                            horizon = horizon, 
                            n_predictors = dim(data$train$x_train)[3], 
                            metrics = metrics[metric][[1]])
      # declare generator
      gen = keras:::as_generator.function(
        batch_generator(x_train=data$train$x_train, 
                        y_train=data$train$y_train, 
                        min_len=min_len, 
                        steps=steps, 
                        horizon=horizon,
                        batchsize = batchsize, 
                        return_weights = T,
                        return_window = F)
      )
      # declare callbacks
      callbacks = callback_early_stopping(monitor = paste0("val_", metric),
                                          mode = if_else(maximize, "max", "min"),
                                          patience = patience,
                                          min_delta = 0.001,
                                          restore_best_weights = TRUE)
      
      history = model %>% fit_generator(gen, 
                                        steps_per_epoch =  max_steps*max_batches, 
                                        epochs = max_epochs, 
                                        verbose = 0 , 
                                        callbacks = callbacks,
                                        validation_data = list(data$val$x_val, data$val$y_val))
      
      
      if(maximize){
        actmodelperf <- max(history$metrics[paste0("val_", metric)][[1]], na.rm=T)
      } else {
        actmodelperf <- min(history$metrics[paste0("val_", metric)][[1]], na.rm=T)
      }
      
      if(isBetter(actmodelperf,bestmodelperf,maximization=maximize)){
        bestmodelperf <- actmodelperf
        bestvars <-  c(startvars,nextvars[i])
      }
      acc <- acc+1
      
      variablenames <-  c(startvars,nextvars[i])
      perf_all[acc,1:length(variablenames)] <- variablenames
      perf_all[acc,(length(vars)+1):ncol(
        perf_all)] <- c(actmodelperf,length(variablenames))
      saveRDS(perf_all, file = file.path(outdir, paste0(pattern, "-perf_all-nextvars.rds")))
      
      if(verbose){
        print(paste0("maximum number of models that still need to be trained during this iteration: ", length(nextvars)-i))
      }
    }
    
    selectedvars <- c(selectedvars, bestvars[!bestvars%in%selectedvars])
    selectedvars_perf <- c(selectedvars_perf, bestmodelperf)
    if(verbose){
      print(paste0(paste0("vars selected: ",paste(selectedvars, collapse = ',')),
                   " with ", metric," ",round(bestmodelperf,3)))
    }
  }
  
  result$selectedvars <- selectedvars
  result$selectedvars_perf <- selectedvars_perf
  result$perf_all <- perf_all
  result$perf_all <- result$perf_all[!apply(is.na(result$perf_all), 1, all),]
  result$perf_all <- result$perf_all[colSums(!is.na(result$perf_all)) > 0]
  result$minVar <- minVar
  result$type <- "ffs"
  saveRDS(result, file = file.path(outdir, paste0(pattern, "-result.rds")))
  return(result)
}
