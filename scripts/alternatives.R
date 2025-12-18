
get.predictions <- function(x, df){
  model1 <- x[[1]]
  model2 <- x[[2]]
  
  new_data1 <- expand_grid(Age = unique(df$Age),
                           Sex = unique(df$Sex),
                           BMI_zscore = 0,
                           id = 50)
  
  ilink1 <- family(model1)$linkinv
  set.seed(1234)
  pred1 <- predict(model1, new_data1, type = "link", se.fit = TRUE, exclude = "s(id)")
  pred1 <- cbind(pred1, new_data1)
  pred1 <- transform(pred1, lwr_ci = ilink1(fit - (2 * se.fit)),
                     upr_ci = ilink1(fit + (2 * se.fit)),
                     fitted = ilink1(fit))
  
  new_data2 <- df %>%
    filter(Sex != "Male") %>% 
    select(RepPhase, Age) %>% 
    mutate(RepGroup = case_when(
      RepPhase %in% c("Cycling","Pregnant","Postpartum") ~ "Reproductive",
      TRUE ~ RepPhase),
      NumPartos = 3,
      BMI_zscore = 0,
      id = 50) %>%
    group_by(RepPhase) %>% 
    distinct(Age, .keep_all = T) %>% 
    select(RepPhase, RepGroup, id, BMI_zscore, Age, NumPartos)

  ilink2 <- family(model2)$linkinv
  set.seed(1234)
  pred2 <- predict(model2, new_data2, type = "link", se.fit = TRUE, exclude = "s(id)")
  pred2 <- cbind(pred2, new_data2)
  pred2 <- transform(pred2, lwr_ci = ilink2(fit - (2 * se.fit)),
                     upr_ci = ilink2(fit + (2 * se.fit)),
                     fitted = ilink2(fit)) 
  pred2 <- pred2  %>%
    mutate(RepPhase = ordered(RepPhase, 
                              levels = c("Pre-Menst",
                                         "Cycling","Pregnant",
                                         "Postpartum",
                                         "Menopause")))
  
  pred <- full_join(pred1, pred2)
  pred
}

test <- get.predictions(model_outputs[["USA.NEU"]], model_matcheddf)

ggplot(test,
       aes(x = Age, y = fit, color = RepPhase, fill = RepPhase)) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci),
              alpha = 0.3, color = NA) +
  geom_line()
