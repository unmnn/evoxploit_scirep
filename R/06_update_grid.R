# extract optimal hyperparameters
files <- dir(file.path(here::here(), "output", "eval"), pattern = paste0("cv_", .config$data_name), full.names = TRUE)
hyper_opt <- map(files, read_rds)
hyper_opt <- map(hyper_opt, function(x) {
  x %>%
      group_by_at(vars(-c(fold, acc))) %>%
      summarize(avg_acc = mean(acc)) %>%
      ungroup() %>%
      arrange(desc(avg_acc)) %>%
      select(-avg_acc) %>%
      slice(1) %>% 
    as.list()
})

grid$opt_par <- vector("list", nrow(grid))
grid$opt_par[as.integer(str_replace(files, ".*?(\\d+)\\.rds$", "\\1"))] <- hyper_opt

# extract performance and best model
files <- dir(file.path(here::here(), "output", "models"), pattern = paste0("opt_", .config$data_name), full.names = TRUE)

res <- map(files, read_rds)
grid$cv <- vector("list", nrow(grid))
grid$cv[as.integer(str_replace(files, ".*?(\\d+)\\.rds$", "\\1"))] <- map(res, ~.$cv)
grid$model <- vector("list", nrow(grid))
grid$model[as.integer(str_replace(files, ".*?(\\d+)\\.rds$", "\\1"))] <- map(res, ~.$model)

file_path <- file.path(here::here(), "output", "grid", str_c("grid_", .config$data_name, ".rds"))
if(!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
write_rds(grid, file_path)
# grid$model[[2]] %>% getLearnerModel() %>% {.$learner.model}

rm(files, res, hyper_opt)
