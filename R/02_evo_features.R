message("02_evo_features.R")
# Create 10 folds ----
folds <- caret::createFolds(dl$meta$label[[1]], 10, returnTrain = TRUE) %>%
  map(function(x){
    idx <- vector(mode = "logical", length = nrow(dl$data))
    idx[x] <- TRUE
    return(idx)
  })

# Create evolution features for all 10 folds and for full dataset ----
file_path <- file.path(here::here(), "output", "evo", paste0("evo_", .config$data_name, ".rds"))
if(!file.exists(file_path)){
  evo <- list()
  evo$all <- Evoxploit$new(data = dl$data, label = dl$meta$label[[1]],
                           wave_suffix = "_s", minPts = NULL,
                           eps = NULL, run = TRUE, verbose = TRUE)
  
  evo$folds <- map(folds, ~Evoxploit$new(data = dl$data, label = dl$meta$label[[1]], train_lgc = .x,
                                         wave_suffix = "_s", minPts = NULL,
                                         eps = NULL, run = TRUE, verbose = TRUE))
  
  if(!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  write_rds(evo, file_path)
} else {
  evo <- read_rds(file_path)
}

message("==========")