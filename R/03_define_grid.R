message("03_define_grid.R")

grid <- tribble(
  ~algo, ~hyper, 
  # glmnet
  # LASSO
  "glmnet", list(alpha = 1, lambda = 10^seq(10, -2, length = 100)),
  # # RIDGE
  "glmnet", list(alpha = 0, lambda = 10^seq(10, -2, length = 100)),
  # kknn
  "kknn", list(k = seq(from = 1, by = 4, length.out = 20)),
  # e1071
  "naiveBayes", list(laplace = 1:5),
  # libsvm
  "svm", list(cost = c(0.01, 0.1, 0.5, 1:3), gamma = 0:3,
              kernel = c("linear", "polynomial", "radial", "sigmoid")),
  # nnet
  "nnet", list(size = seq(1,13,2), decay = 10^-(seq(4,1)), MaxNWts = 100000),
  # "plsdaCaret", list(ncomp = 1:5),
  # rpart
  "rpart", list(cp = c(0.001, 0.005, 0.01, 0.05, 0.1)),
  # C50
  "C50", list(winnow = c(winnow_false = FALSE, winnow_true = TRUE),
              CF = seq(0,0.35,0.05),
              rules = c(rules_false = FALSE, rules_true = TRUE)),
  # ranger
  "ranger", list(mtry = c(1,2,4,8), min.node.size = c(1, seq(5,25,5)), num.trees = c(250, 500, 1000)),
  # "h2o.randomForest", list(mtries = seq(4, min(100, ncol(dl$data)-1), 16)),
  # xgboost
  "xgboost", list(eta = c(0.01, 0.05, 0.1, 0.2), max_depth = 1:3, gamma = 0, colsample_bytree = seq(0.2, 1, 0.2),
                  min_child_weight = c(0.5, 1, 2), subsample = seq(0.5, 1, 0.25), nrounds = seq(50, 250, 100))
) %>%
  mutate(subset = list(c(#"original", "evo", "sumstats", 
    "original+evo"#, "original+sumstats", "all"
  ))) %>%
  mutate(fs = list(c("none", "cfs"))) %>%
  unnest(subset, .drop = FALSE) %>%
  unnest(fs, .drop = FALSE) %>%
  mutate(id = str_pad(row_number(), width = 3, pad = "0")) %>%
  # mutate(fold = list(1:10)) %>%
  # unnest(fold, .drop = FALSE) %>%
  select(id, everything())

grid
# getParamSet("classif.nnet")
message("==========")
