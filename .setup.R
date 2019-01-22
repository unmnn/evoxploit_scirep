if(!"pacman" %in% installed.packages()) install.packages("pacman")

install_cran <- {setdiff(c(
  "C50",
  "caret",
  "dummies",
  "e1071",
  "extrafont",
  "glmnet",
  "here",
  "kknn",
  "janitor",
  "mailR",
  "mlr",
  "nnet",
  "pls",
  "randomForest",
  "ranger",
  "rJava",
  "rpart",
  "tidyverse",
  "xgboost"
), installed.packages())}

if(length(install_cran) > 0) pacman::p_install(install_cran, character.only = TRUE)
rm(install_cran)

github_pcks <- c(
  "thomasp85/lime",
  "thomasp85/patchwork",
  "unmnn/evoxploit"
)

if(length(github_pcks) > 0) {
  names(github_pcks) <- gsub(".*/(.*)", "\\1", github_pcks)
  install_github <- setdiff(names(github_pcks), installed.packages())
  if(length(install_github) > 0) pacman::p_install_gh(github_pcks[install_github])
  rm(install_github)
}

rm(github_pcks)

# extrafont::font_import()
extrafont::loadfonts("win", quiet = TRUE)

.opt <- list()

.opt$algo_names <- c(`C50` = "C5.0",
                     `glmnet` = "LASSO/RIDGE",
                     `rpart` = "CART",
                     `kknn` = "WKNN",
                     `naiveBayes` = "Naive\nBayes",
                     `nnet` = "MLP",
                     `plsdaCaret` = "Partial\nLeast\nSquares",
                     `ranger` = "Random\nForest",
                     `svm` = "SVM",
                     `xgboost` = "Gradient\nBoosted\nTrees")

.opt$plot <- list()
.opt$plot$base_size <- 12
# .opt$plot$base_family <- "Fira Sans"

ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = .opt$plot$base_size, 
                         base_family = .opt$plot$base_family) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) + 
    ggplot2::theme(axis.line = ggplot2::element_line()) + 
    ggplot2::theme(axis.ticks = ggplot2::element_line())
)
