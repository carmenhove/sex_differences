
#PREDICTED VALUES
pred_list <- map(model_outputs, ~ get.predictions(., model_matcheddf))

pred_df <- bind_rows(pred_list, .id = "Population.Measure") %>%
  separate(Population.Measure, c("Population","Measure")) %>%
  mutate(Unit = case_when(Measure !='NLR' ~ "Cells/µL", TRUE ~ "Ratio")) %>% 
  group_by(Age, Population, Measure)  %>% 
  filter(Age >= 2, Age <= 84) %>% 
  group_by(Age, Population, Measure) %>%
  mutate(fit = case_when(Unit == "Ratio" ~ round(fit, digits = 2), TRUE ~ round(fit)),
         lwr_ci = case_when(Unit == "Ratio" ~ round(lwr_ci, digits = 2), TRUE ~ round(lwr_ci)),
         upr_ci = case_when(Unit == "Ratio" ~ round(upr_ci, digits = 2), TRUE ~ round(upr_ci)),
         Male_median = fit[Sex == "Male"],
         Male_lwr = lwr_ci[Sex == "Male"], 
         Male_upr = upr_ci[Sex == "Male"],
         Measure = ordered(Measure, levels = c("WBC","NEU","LYM","NLR","MON","EOS")))

## Can now set parity to desired number in plots to highlight effects of parity
pred_parity_list <- split(pred_df, list(pred_df$NumPartos))

pred_parity_df <- map(pred_parity_list, get.dfs) %>% 
  bind_rows(., .id = "NumPartos")

## Effects of parity
parity_effects_list <- map(model_outputs, ~get.predictions.parity(., model_matcheddf))

parity_effects_df <- bind_rows(parity_effects_list, .id = "Population.Measure") %>%
  separate(Population.Measure, c("Population","Measure")) %>%
  mutate(Unit = case_when(Measure !='NLR' ~ "Cells/µL", TRUE ~ "Ratio"),
         Measure = ordered(Measure, levels = c("WBC","NEU","LYM","NLR","MON","EOS")))
