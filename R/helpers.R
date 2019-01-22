harmonize_factors <- function(df, wave_suffix = "_s") {
  stem_fct <- evoxploit::get_global_attname_stem(names(df %>% keep(is.factor)), min_wave_count = 2)
  
  df_append <- map_dfc(stem_fct, function(x){
    dl_sub <- df %>% select(starts_with(str_c(!!x, wave_suffix)))
    all_levels <- unique(reduce(map(dl_sub, levels), c))
    dl_sub <- dl_sub %>% mutate_all(funs(factor(., levels = all_levels)))
    return(dl_sub)
  })
  
  df[names(df_append)] <- df_append
  return(df)
}

# Remove near-zero-variance attributes ----
remove_near_zero_var <- function(df) {
  idx_nz <- caret::nearZeroVar(df, freqCut = 20)
  if(length(idx_nz) > 0) df <- df[-idx_nz]
  df
}

# Remove attributes with more than 10 % missing values and...
# ...replace missing values with mean and mode
remove_high_missingness_ratio_atts <- function(df) {
  missing_perc <- sort(purrr::map_dbl(df, ~ mean(is.na(.x))))
  df_new <- df[missing_perc < 0.1]
  if(ncol(df_new) == 0) {
    browser()
  }
  df_new <- randomForest::na.roughfix(df_new)
  return(df_new)
}



send_mail <- function(time_start, time_end, perc_successful = 1) {
  dt <- difftime(.time_end, .time_start, units = "auto")
  
  .cred <- read_csv(".smtp_credentials.txt")
  recipients <- .cred %>% filter(key == "recipent") %>% pull(value)
  mail_subject <- paste0("Rupture classification ", round(100 * perc_successful), "% successfull")
  mail_body <- paste0("Start:", time_start, "\n",
                      "End:", time_end, "\n",
                      "Duration:", round(dt, 2), " ", attr(dt, "units"))
  email <- mailR::send.mail(from = .cred %>% filter(key == "sender") %>% pull(value),
                     to = recipients,
                     subject = mail_subject,
                     body = mail_body,
                     smtp = list(host.name = .cred %>% filter(key == "host.name") %>% pull(value), 
                                 port = .cred %>% filter(key == "port") %>% pull(value) %>% as.integer(), 
                                 tls = TRUE,
                                 user.name = .cred %>% filter(key == "user.name") %>% pull(value), 
                                 passwd = .cred %>% filter(key == "passwd") %>% pull(value)), 
                     authenticate = TRUE,
                     send = TRUE)
}
# send_mail(.time_start, .time_end)