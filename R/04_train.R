message("04_train.R")

run_classification_folds <- function(data, tbl_label, evo, folds, run_id, algo, hyper, fs, subset, add, overwrite = TRUE) {
  file_path <- file.path(here::here(), "output", "eval", paste0("cv_", add, "_", run_id, ".rds"))
  
  if(!overwrite){
    if(file.exists(file_path)) {
      message(paste0("Skipping run ", run_id, ". File already exists."))
      return(invisible())
    }
  }
  
  learner <- makeLearner(paste0("classif.", algo), predict.type = "prob", fix.factors.prediction = TRUE)
  
  if(fs == "cfs") { learner <- makePreprocWrapper(learner, train = cfs_trainfun, predict = cfs_predictfun)}
  
  ps <- expand.grid(hyper, stringsAsFactors = FALSE)
  # ps <- transpose(ps)
  ps <- split(ps, rownames(ps))
  ps <- map(ps, as.list)
  
  measures <- list(acc, kappa)
  
  res <- tibble()
  
  for(f in 1:length(folds)) {
    message(str_c("fold ", f))
    if(subset == "original") df_train <- bind_cols(tbl_label, dl$data)
    if(subset == "evo") df_train <- bind_cols(tbl_label, evo$folds[[f]]$evo_features %>% select(-starts_with("desc_")))
    if(subset == "sumstats") df_train <- bind_cols(tbl_label, evo$folds[[f]]$evo_features %>% select(starts_with("desc_")))
    if(subset == "original+evo") df_train <- bind_cols(tbl_label, dl$data, evo$folds[[f]]$evo_features %>% select(starts_with("desc_")))
    if(subset == "original+sumstats") df_train <- bind_cols(tbl_label, dl$data, evo$folds[[f]]$evo_features %>% select(starts_with("desc_")))
    if(subset == "all") df_train <- data
    names(df_train)[1] <- "response"
    df_train <- remove_near_zero_var(df_train)
    df_train <- remove_high_missingness_ratio_atts(df_train)
    
    if(algo %in% c("xgboost")) {
      # Apply One Hot Encoding to convert all multinomial factors into binary factors
      df_train <- bind_cols(df_train[1], dummy.data.frame(as.data.frame(df_train[-1]), sep = "=", verbose = FALSE))
      # names(df_train) <- str_replace(names(df_train), "=(.*)", "='\\1'")
      df_train <- janitor::clean_names(df_train)
      
    }
    
    task <- makeClassifTask(data = as.data.frame(df_train), target = "response")
    
    # custom train/test
    rin <- makeFixedHoldoutInstance(train.inds = which(folds[[f]]), test.inds = which(!folds[[f]]), size = nrow(df_train))
    
    res <- res %>% bind_rows(
      map_dfr(ps, function(y){
        learner_resample <- setHyperPars(learner, par.vals = y)
        r <- NULL
        try({
          r <- resample(learner_resample, task = task, resampling = rin, measures = measures, models = FALSE, keep.pred = FALSE, show.info = FALSE)
          parms <- unnest(spread(enframe(y), name, value))
        })
        if(!is.null(r)) {
          return(tibble(fold = f) %>%
                   bind_cols(tibble(acc = r$measures.test["acc"][[1]])) %>%
                   bind_cols(parms)
          )
        }
        return(r)
      })
    )
  }
  
  if(!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  write_rds(res, file_path)
  return(invisible())
}

run_opt <- function(data, tbl_label, evo, folds, run_id, algo, ps, fs, subset, add, overwrite = TRUE) {
  
  file_path <- file.path(here::here(), "output", "models", paste0("opt_", add, "_", run_id, ".rds"))
  
  if(!overwrite){
    if(file.exists(file_path)) {
      message(paste0("Skipping run ", run_id, ". File already exists."))
      return(invisible())
    }
  }
  
  
  learner <- makeLearner(paste0("classif.", algo), predict.type = "prob", fix.factors.prediction = TRUE)
  
  if(fs == "cfs") { learner <- makePreprocWrapper(learner, train = cfs_trainfun, predict = cfs_predictfun)}
  
  # ps is list
  learner_resample <- setHyperPars(learner, par.vals = ps)
  
  measures <- list(acc, kappa)
  
  res <- tibble()
  
  for(f in 1:length(folds)) {
    message(str_c("fold ", f))
    if(subset == "original") df_train <- bind_cols(tbl_label, dl$data)
    if(subset == "evo") df_train <- bind_cols(tbl_label, evo$folds[[f]]$evo_features %>% select(-starts_with("desc_")))
    if(subset == "sumstats") df_train <- bind_cols(tbl_label, evo$folds[[f]]$evo_features %>% select(starts_with("desc_")))
    if(subset == "original+evo") df_train <- bind_cols(tbl_label, dl$data, evo$folds[[f]]$evo_features %>% select(starts_with("desc_")))
    if(subset == "original+sumstats") df_train <- bind_cols(tbl_label, dl$data, evo$folds[[f]]$evo_features %>% select(starts_with("desc_")))
    if(subset == "all") df_train <- data
    names(df_train)[1] <- "response"
    df_train <- remove_near_zero_var(df_train)
    df_train <- remove_high_missingness_ratio_atts(df_train)
    
    if(algo %in% c("xgboost")) {
      # Apply One Hot Encoding to convert all multinomial factors into binary factors
      df_train <- bind_cols(df_train[1], dummy.data.frame(as.data.frame(df_train[-1]), sep = "=", verbose = FALSE))
      # names(df_train) <- str_replace(names(df_train), "=(.*)", "='\\1'")
      df_train <- janitor::clean_names(df_train)
      
    }
    
    task <- makeClassifTask(data = as.data.frame(df_train), target = "response")
    
    # custom train/test
    rin <- makeFixedHoldoutInstance(train.inds = which(folds[[f]]), test.inds = which(!folds[[f]]), size = nrow(df_train))
    
    r <- NULL
    try({
      r <- resample(learner_resample, task = task, resampling = rin, measures = measures, models = TRUE, keep.pred = TRUE, show.info = FALSE)
    })
    if(!is.null(r)) {
      res <- bind_rows(res, (tibble(fold = f) %>%
                               bind_cols(tibble(acc = r$measures.test["acc"][[1]],
                                                kappa = r$measures.test["kappa"][[1]])) %>%
                               # bind_cols(parms)
                               mutate(model = list(r$models)[[1]]) %>%
                               mutate(pred = list(r$pred)))
      )
    }
  }
  
  # train on full training set
  if(subset == "original") df_train <- bind_cols(tbl_label, dl$data)
  if(subset == "evo") df_train <- bind_cols(tbl_label, evo$all$evo_features %>% select(-starts_with("desc_")))
  if(subset == "sumstats") df_train <- bind_cols(tbl_label, evo$all$evo_features %>% select(starts_with("desc_")))
  if(subset == "original+evo") df_train <- bind_cols(tbl_label, dl$data, evo$all$evo_features %>% select(starts_with("desc_")))
  if(subset == "original+sumstats") df_train <- bind_cols(tbl_label, dl$data, evo$all$evo_features %>% select(starts_with("desc_")))
  if(subset == "all") df_train <- data
  names(df_train)[1] <- "response"
  df_train <- remove_near_zero_var(df_train)
  df_train <- remove_high_missingness_ratio_atts(df_train)
  
  if(algo %in% c("xgboost")) {
    # Apply One Hot Encoding to convert all multinomial factors into binary factors
    df_train <- bind_cols(df_train[1], dummy.data.frame(as.data.frame(df_train[-1]), sep = "=", verbose = FALSE))
    # names(df_train) <- str_replace(names(df_train), "=(.*)", "='\\1'")
    df_train <- janitor::clean_names(df_train)
    
  }
  task <- makeClassifTask(data = as.data.frame(df_train), target = "response")
  
  model <- NULL
  try({model <- train(learner_resample, task)})
  
  if(!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  write_rds(list(cv = res, model = model), file_path)
  return(invisible())
}

message("==========")