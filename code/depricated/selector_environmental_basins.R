source("src/setup.R")
source("src/modelling/utils.R")
seed = 42
unit = "basins"
type = "environmental"

conflicts = readRDS(file.path(envrmt$env_vector, "response_cube.rds"))
predictors = readRDS(file.path(envrmt$env_vector, "predictor_cube.rds"))

conflicts %>%
  filter(unit == "basins", time > as.Date("2000-12-01")) -> conflicts
predictors %>%
  filter(unit == "basins", time > as.Date("2000-12-01")) -> predictors

predictors %>% filter(id == 1, time == as.Date("2010-01-01")) %>% as_tibble() %>% pull(var) %>% unique %>% sort -> vars
log_vars = vars[grep("GDP|LVSTK|POP|TRI|TRT", vars)]

# take natural log for log vars
predictors %>%
  as_tibble() %>%
  filter(var %in% log_vars) %>%
  mutate(value =  log(value)) -> log_vals

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
  mutate(value = replace_na(value, -1)) %>%
  dplyr::select(-xmax, -xmin) %>%
  as.tbl_cube(dim_names = 1:3) -> predictors

dir.create(file.path(envrmt$env_ffs, "tmp"), showWarnings = F)
# loop through response variables
vars = c("all", "sb", "ns", "os")
for (var in vars){
  print(paste0("Starting FFS for response variable ", var, "."))
  ffs_basline = readRDS(file.path(envrmt$env_ffs, paste0("ffs_result-baseline-", unit, "-", var, ".rds")))
  selectedvars = ffs_basline$selectedvars
  paras = readRDS(file.path(envrmt$env_bayes, paste0(paste("paras", type, unit, sep = "-"), ".rds")))
  ffs_result = ffs(response = conflicts,
                   predictors = predictors,
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
                   thres = 0,
                   selectedvars = selectedvars)
  saveRDS(ffs_result, file.path(envrmt$env_ffs, paste0(paste("ffs_result", type, unit, var, sep = "-"), ".rds")))
}
