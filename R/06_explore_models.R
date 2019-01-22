library("iml")
library("patchwork")
# Random-Forest-esque Feature Importance ----
# interpretation: relation between model without the feature and model with the feature 
selected_model <- tg %>% filter(algo %in% c("ranger", "xgboost"))

for (i in 1:nrow(selected_model)) {
  predictor <- Predictor$new(model = selected_model$model[[i]],
                             data = df_train %>% select(-response),
                             y = df_train$response)
  
  df_imp <- map_dfr(1:5, function(x){
    imp <- FeatureImp$new(predictor, loss = "rmse", n.repetitions = 1)
    imp$results %>%
      mutate(rep_id = x)
  })
  
  df_plot <- df_imp %>%
    group_by(feature) %>%
    summarize(med_fi = median(importance),
              mad_fi = mad(importance)) %>%
    ungroup() %>%
    mutate(feature = fct_reorder(feature, med_fi)) %>%
    arrange(desc(abs(med_fi)))
  
  
  (p <- ggplot(df_plot, aes(x = feature)) +
    geom_hline(yintercept = 1, color = "black") +
    geom_point(aes(y = med_fi), color = topt$mycols[["purple3"]]) +
    geom_segment(aes(xend = feature, y = med_fi - mad_fi, yend = med_fi + mad_fi), color = topt$mycols[["purple3"]]) +
    ggrepel::geom_text_repel(data = df_plot %>% slice(1:10), aes(y = med_fi, label = feature),
                             family = topt$base_family) +
    coord_flip() + 
    labs(y = "Feature Importance (5 Iterations)", x = NULL) +
    theme(panel.grid.major.y = element_blank()) +
    theme(axis.text.y = element_blank()) +
    theme(axis.ticks.y = element_blank()))
  
  write_rds(p, file.path(here::here(), "03_Reg", "output", "figures", paste0(selected_model$algo[[i]], "_fi.rds")))
}

# confusion matrix ----
a <- tg %>%
  filter(algo == "ranger") %>%
  pluck("r", 1, "pred", "data") %>%
  filter(set == "test") %>%
  select(id, truth_E = truth, response_E = response) %>%
  bind_cols(df_train %>% select(truth_A = !!response)) %>%
  gather(key, value, -id) %>%
  mutate(value = cut(value, breaks = c(-Inf, 15, Inf), labels = c("n", "y"))) %>%
  spread(key, value) %>%
  mutate(truth_AE = paste0(truth_A, truth_E)) %>%
  count(truth_AE, response_E) %>%
  spread(response_E, n) %>%
  mutate(ratio_correctly_classified = if_else(str_sub(truth_AE, 2, 2) == "n", n / (n + y), y / (n + y)))


tt <- tg %>%
  filter(algo == "xgboost") %>%
  pluck("r", 1, "pred", "data") %>%
  filter(set == "test") %>%
  select(id, truth, response) %>%
  # gather(key, value, -id) %>%
  # mutate(value = cut(value, breaks = c(-Inf, 15, Inf), labels = c("n", "y"))) %>%
  # spread(key, value) %>%
  select(id, truth_E = truth, response_E = response)
caret::confusionMatrix(data = tt$response_E, reference = tt$truth_E, positive = "y")

# caret::confusionMatrix(data = a$response_E, reference = a$truth_AE)

# baseline model: just predict response at A
# sqrt(mean((df_train$adsl_adsl_sum - df_train$response)^2))
# sqrt(mean((mean(df_train$response) - df_train$response)^2)) # mean
test <- tibble(
  a = df_train$adsl_adsl_sum %>% cut(breaks = c(-Inf, 15, Inf), labels = c("depression-no", "depression-yes")),
  e = df_train$response %>% cut(breaks = c(-Inf, 15, Inf), labels = c("depression-no", "depression-yes"))
)
caret::confusionMatrix(data = test$a, reference = test$e, positive = "depression-yes")


## Lasso: How many times was a feature weight above 0 in CV with best parameter setting? ----
selected_model <- tg %>%
  filter(algo == "glmnet") %>%
  arrange(rmse.test.rmse) %>%
  slice(1) %>%
  pluck("r", 1, "models")

a <- imap_dfr(selected_model, function(x,y){
  tmp_coeffs <- coef(x$learner.model, s = "lambda.min")
  tbl_coefs <- tibble(fold = y, name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
}) %>%
  filter(!name == "(Intercept)")

df_plot <- a %>%
  group_by(name) %>%
  summarize(n = n()/10, 
            med_coeff = median(coefficient),
            mad_coeff = mad(coefficient)) %>%
  ungroup() %>%
  arrange(desc(n), desc(abs(med_coeff))) %>%
  mutate(rn = row_number()) %>%
  mutate(name = fct_reorder(name, rn, .desc = TRUE))

(p1 <- ggplot(df_plot, aes(x = name)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    geom_col(aes(y = n), fill = topt$mycols[["grey"]]) +
    coord_flip() +
    labs(y = "Avg. selection in 10-fold CV", x = NULL) +
    theme(plot.margin = margin(0.1,2,0.5,0.5, "lines")) +
    theme(panel.grid.major.y = element_blank())
)


(p2 <- ggplot(df_plot, aes(x = name)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(aes(y = med_coeff)) +
    geom_segment(aes(xend = name, y = med_coeff - mad_coeff, yend = med_coeff + mad_coeff)) +
    coord_flip() + 
    labs(y = "LASSO coefficient", x = NULL) +
    theme(panel.grid.major.y = element_blank()) +
    theme(axis.text.y = element_blank()) +
    theme(axis.ticks.y = element_blank())
)

(p <- p1 + p2 + patchwork::plot_layout(nrow = 1, widths = c(0.3, 0.7)))
write_rds(p, file.path(here::here(), "03_Reg", "output", "figures", "lasso_coeffs.rds"))

# residual plot age ----
(p1 <- ggplot(df_train, aes(x = .age, y = response)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age", y = response)
)

(p2 <- tg %>% 
    filter(algo == "xgboost") %>%
    pluck("r", 1, "pred", "data") %>%
    filter(set == "test") %>%
    mutate(resid = (truth - response)) %>%
    inner_join(df_train %>% mutate(id = row_number()) %>% select(id, .age), by = "id") %>%
    ggplot(aes(x = .age, y = resid)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Age", y = "Residual")
  )

(p <- p1 + p2 + plot_layout(nrow = 1))
write_rds(p, file.path(here::here(), "03_Reg", "output", "figures", "xgboost_age_resid.rds"))
