message("01_prepare.R")

# Load file ----
if(.config$data_name == "epi") {
  dl <- list()
  dl$data <- epi$data
  dl$meta <- list()
  dl$meta$label <- epi$label
} else {
  dl <- read_rds(file.path(here::here(), "data", str_c(.config$data_name, ".rds")))
}

# Remove near-zero-variance attributes ----
dl$data <- remove_near_zero_var(dl$data)

# Remove attributes with more than 10 % missing values ----
# replace missing values with mean and mode
dl$data <- remove_high_missingness_ratio_atts(dl$data)

# Lump together factor levels that appear less than 5% ----
# dl$data %>% keep(is.factor) %>% map(~table(.x) %>% prop.table)
dl$data <- lump_levels(dl$data)

# Harmonize factor levels ----
dl$data <- harmonize_factors(dl$data)

message("==========")