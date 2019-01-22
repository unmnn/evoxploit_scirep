if(.config$data_name == "epi") {
  dl <- epi
  dl$meta <- list()
  dl$meta$label <- dl$label
} else {
  dl <- read_rds(file.path(here::here(), "data", str_c(.config$data_name, ".rds")))
}


# Lump together factor levels that appear less than 5% ----
# dl$data %>% keep(is.factor) %>% map(~table(.x) %>% prop.table)
dl$data <- dl$data %>% map_if(is.factor, function(x, prop){
  tb <- prop.table(table(as.character(x)))
  bt <- which(tb < prop)
  if(length(bt) > 1) x <- fct_lump(x, prop = prop, other_level = "other")
  x
}, prop = 0.05) %>%
  bind_cols()

# Remove near-zero-variance attributes ----
dl$data <- remove_near_zero_var(dl$data)

# Remove attributes with more than 10 % missing values ----
# replace missing values with mean and mode
dl$data <- remove_high_missingness_ratio_atts(dl$data)

# Harmonize factor levels ----
dl$data <- harmonize_factors(dl$data)
