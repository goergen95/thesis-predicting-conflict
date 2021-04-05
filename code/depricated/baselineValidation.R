# baseline evaluation
library(stringr)
library(MLeval)
library(dplyr)
library(tidyr)
library(ggplot2)

model_files = list.files("output/models/", full.names = T)
data_files = list.files("output/validation/", full.names = T)


split_names = str_split(basename(model_files),"_")
model_info = lapply(split_names, function(x){
  data.frame(category = x[1], unit = x[2], type=x[3], month = x[4], rsmp = str_sub(x[5], 0, -5))
  })
model_info = do.call(rbind, model_info)

validation = lapply(1:length(model_files),function(i){
 model = readRDS(model_files[i])
 data = readRDS(data_files[grep(paste(model_info[i,c(2,3,4)], collapse = "_"), data_files)])
 
 pred = predict(model, data, type = "prob")
 pred$obs = factor(data$response, levels = c(1,0), labels = c("conflict", "peace"))
 evaluation = evalm(pred, positive = "conflict", silent = TRUE, plots = "")
 metric_names = c(rownames(evaluation$stdres$Group1), "BS")
 vals = c(evaluation$stdres$Group1$Score, brier_score(pred, "conflict"))
 metrics = data.frame(var = metric_names, value = vals)
 metrics$category = model_info[i,1]
 metrics$unit = model_info[i,2]
 metrics$type = model_info[i,3]
 metrics$month = as.numeric(str_sub(model_info[i,4],0,-2))
 metrics$rsmp = model_info[i,5]
 return(metrics)
 })


validation = do.call(rbind, validation)
saveRDS(validation, "output/MLeval/baseline_validation.rds")


metrics = unique(validation$var)
metrics = c("AUC-ROC", "AUC-PR", "BS", "F1")
baseunits = c("basins", "states")

for(u in baseunits){
  for(m in metrics){
    
    baseunit = u
    metric = m
    
    plt = validation %>%
      as_tibble %>%
      filter(unit == baseunit & var == metric) %>%
      mutate(month = factor(month, levels = 1:12, labels = paste(1:12, "m", sep = "") )) %>%
      #filter(var == metric) %>%
      ggplot(aes(x = month, y = value)) +
      geom_point(aes(color=rsmp))+
      facet_wrap(~type, nrow = 3, ncol = 1)+
      ylab(metric)+
      xlab("Prediction n months into the future") +
      labs(color = "Resampling\nmethod")+
      ggtitle(paste0("Baseline validation for the year 2018 - Unit of analysis: ", baseunit))+
      theme_minimal()
    
    metric = str_replace_all(metric," ", "-")
    ggsave(filename = file.path("output/plots", paste0("baseline_",baseunit,"_",metric,".png")))
  }
}
