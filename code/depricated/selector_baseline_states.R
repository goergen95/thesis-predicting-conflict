source("src/setup.R")
source("src/modelling/utils.R")

seed = 42
unit = "states"
type = "baseline"

conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))

conflicts %<>%
  filter(unit == "states", time > as.Date("2000-12-01"), id <= 847)

dir.create(file.path(envrmt$env_ffs, "tmp"), showWarnings = F)
# loop through response variables
vars = c("all", "sb", "ns", "os")
for (var in vars){
  print(paste0("Starting FFS for response variable ", var, "."))
  paras = readRDS(file.path(envrmt$env_bayes, paste0(paste("paras", type, unit, sep = "-"), ".rds")))
  ffs_result = ffs(response = conflicts,
                   predictors = NULL,
                   y_var = var, 
                   metric = "f2",
                   maximize = TRUE,
                   minVar = 2, 
                   seed = seed,
                   paras = paras,
                   verbose = T,
                   outdir = file.path(envrmt$env_ffs, "tmp/"),
                   pattern =  paste(type, unit, var, sep = "-"),
                   horizon = 12,
                   steps = 4,
                   batchsize = 200,
                   min_len = 24,
                   patience = 10,
                   max_epochs = 200,
                   shuffle = T,
                   thres = 0)
saveRDS(ffs_result, file.path(envrmt$env_ffs, paste0(paste0("ffs_result", type, unit, var, sep = "-"), ".rds")))
}
