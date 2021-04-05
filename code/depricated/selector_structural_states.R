source("src/setup.R")
source("src/modelling/utils.R")
seed = 42
unit = "states"
type = "structural"

conflicts = readRDS("data/vector/response_cube.rds")
predictors = readRDS("data/vector/predictor_cube.rds")

conflicts %>%
  filter(unit == "states", time > as.Date("2000-12-01"), id <= 847) -> conflicts
predictors %>%
  filter(unit == "states", time > as.Date("2000-12-01"), id <= 847) -> predictors


predictors %>% filter(id == 1, time == as.Date("2010-01-01")) %>% as_tibble() %>% pull(var) %>% unique %>% sort -> vars
target_vars = vars[grep("DEP|GDP|BARE|CROP|FOREST|GRASS|SHRUB|URBAN|WATER|LVSTK|POP|TRI|TRT", vars)]
log_vars = vars[grep("GDP|LVSTK|POP|TRI|TRT", vars)]

predictors %>%
  filter(var %in% target_vars) -> predictors

# take natural log for log vars
predictors %<>%
  as_tibble() %>%
  mutate(value = if_else(var %in% log_vars, log(value), value))


# calculate normalistaion factors xmin and xmax based on data up to 2017
predictors %>%
  filter(time > as.Date("2000-12-31"),
         time <= as.Date("2017-12-31")) %>%
  #  as_tibble() %>%
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
