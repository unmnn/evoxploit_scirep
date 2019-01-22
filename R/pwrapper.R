# library(tidyverse)
# library(mlr)
# library(FSelector)

cfs_trainfun = function(data, target, args = list()) {
  rf_names <- FSelector::cfs(as.formula(paste0(target, "~.")), data = data)
  control <- list(rf_names = rf_names)
  data <- data[, c(target, rf_names), drop = FALSE]
  return(list(data = data, control = control))
  
  # # Identify numerical features
  # cns = colnames(data)
  # nums = setdiff(cns[sapply(data, is.numeric)], target)
  # # Extract numerical features from the data set and call scale
  # x = as.matrix(data[, nums, drop = FALSE])
  # x = scale(x, center = args$center, scale = args$scale)
  # # Store the scaling parameters in control
  # # These are needed to preprocess the data before prediction
  # control = args
  # if (is.logical(control$center) && control$center)
  #   control$center = attr(x, "scaled:center")
  # if (is.logical(control$scale) && control$scale)
  #   control$scale = attr(x, "scaled:scale")
  # # Recombine the data
  # data = data[, setdiff(cns, nums), drop = FALSE]
  # data = cbind(data, as.data.frame(x))
  # return(list(data = data, control = control))
}

cfs_predictfun = function(data, target, args, control) {
  data <- data[, control$rf_names, drop = FALSE]
  # # Identify numerical features
  # cns = colnames(data)
  # nums = cns[sapply(data, is.numeric)]
  # # Extract numerical features from the data set and call scale
  # x = as.matrix(data[, nums, drop = FALSE])
  # x = scale(x, center = control$center, scale = control$scale)
  # # Recombine the data
  # data = data[, setdiff(cns, nums), drop = FALSE]
  # data = cbind(data, as.data.frame(x))
  # return(data)
}

# lrn <- makeLearner("regr.rpart", cp = 0.1)
# lrn <- makePreprocWrapper(lrn, train = cfs_trainfun, predict = cfs_predictfun)
# lrn
# 
# rdesc <- makeResampleDesc("CV", iters = 3)
# r <- resample(lrn, bh.task, resampling = rdesc, show.info = FALSE)
# r
# 
# a <- train(lrn, makeRegrTask(data = MASS::Boston, target = "medv"))
# # a$learner.model$control