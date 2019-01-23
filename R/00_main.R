set.seed(2019)
source(file.path(here::here(), ".setup.R"), encoding = "UTF-8")

options(parallelMap.default.show.info = FALSE)

library("tidyverse")
library("evoxploit")
library("parallelMap")
library("mlr")
library("dummies") # if dummies hasn't been attached it throws an error on usage for some reason

# Load additional helper functions
source(file.path(here::here(), "R", "pwrapper.R"), encoding = "UTF-8")
source(file.path(here::here(), "R", "helpers.R"), encoding = "UTF-8")

# SET NAME OF DATASET HERE
.config <- list()
.config$data_name <- c("ship_hepstea_all",
                       "ship_goiter_all",
                       "ship_gallstone_all",
                       "monica_all",
                       "epi")[1]
# if file with SMTP credentials exists, send a mail after executing all experiments
.config$send_mail <- ".smtp_credentials.txt" %in% list.files(all.files = TRUE)

source(file.path(here::here(), "R", "01_prepare.R"), encoding = "UTF-8")
source(file.path(here::here(), "R", "02_evo_features.R"), encoding = "UTF-8")
source(file.path(here::here(), "R", "03_define_grid.R"), encoding = "UTF-8")
source(file.path(here::here(), "R", "04_train.R"), encoding = "UTF-8")
source(file.path(here::here(), "R", "05_run_grid.R"), encoding = "UTF-8")
source(file.path(here::here(), "R", "06_update_grid.R"), encoding = "UTF-8")
