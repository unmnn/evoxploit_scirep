# data <- bind_cols(dl$meta$label, dl$data, evo$evo_features)
# names(data)[1] <- "response"
# write_rds(file.path(here::here(), "output", "data", .config$data_name))

.time_start <- Sys.time()
grid$flag_success <- FALSE
parallelStartSocket(3)

for(i in 1:nrow(grid)) { 
  message(paste("Grid id", grid$id[i], "-", grid$algo[i], grid$subset[i], grid$fs[i]))
  try({
    run_classification_folds(data = dl$data,
                             tbl_label = dl$label,
                             evo = evo,
                             folds = folds,
                             run_id = grid$id[i],
                             algo= grid$algo[i],
                             hyper = grid$hyper[[i]],
                             fs = grid$fs[i],
                             subset = grid$subset[i],
                             add = .config$data_name,
                             overwrite = FALSE)

    grid$flag_success[i] <- TRUE
  })
}

# browser()

for(i in 1:nrow(grid)) {
  file_path <- file.path(here::here(), "output", "eval", paste0("cv_", .config$data_name, "_", grid$id[i], ".rds"))
  if(file.exists(file_path)) {
    # extract opt parameter settings
    hy <- read_rds(file_path)
    opt_par <- hy %>%
      group_by_at(vars(-c(fold, acc))) %>%
      summarize(avg_acc = mean(acc)) %>%
      ungroup() %>%
      arrange(desc(avg_acc)) %>%
      select(-avg_acc) %>%
      slice(1)
    
    try({
      message(paste("Grid id", grid$id[i], "-", grid$algo[i], grid$subset[i], grid$fs[i]))
      run_opt(data = dl$data,
              tbl_label = dl$label,
              evo = evo,
              folds = folds,
              run_id = grid$id[i],
              algo= grid$algo[i],
              ps = as.list(opt_par),
              fs = grid$fs[i],
              subset = grid$subset[i],
              add = .config$data_name,
              overwrite = FALSE)
      
    })
  }
}

parallelStop()
.time_end <- Sys.time()
(difftime(.time_end, .time_start, units = "auto"))

send_mail(.time_start, .time_end, mean(grid$flag_success))

rm(hy, i, opt_par)