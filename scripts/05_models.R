
#MODELS

model_df <- merged_df %>%
  ungroup(pid) %>%
  mutate(RepPhase = factor(RepPhase), Sex = factor(Sex)) %>%
  select(RepPhase, Sex, id, BMI, Population, Age, measure, NumPartos, value, MonthPP, MonthPreg) %>%
  filter(measure != "BAS", measure != "crp", !is.na(BMI), Age <= 85, (!is.na(NumPartos) | Sex=="Male"),
         !(RepPhase == "Postpartum" & NumPartos == 0)) %>% 
  mutate(Age_group = case_when(Age >= 0 & Age <=20 ~ as.character(Age), TRUE ~ "20+")) %>%
  group_by(Sex, Age) %>%
  mutate(BMI_zscore = (BMI - mean(BMI))/sd(BMI))

trim_values <- merged_df %>%
  group_by(Population, measure) %>%
  summarise(maximum = quantile(value, probs = 0.99),
            minimum = quantile(value, probs = 0.01))
         
model_list_untrimmed <- split(model_df, list(model_df$Population, model_df$measure))

model_list_trimmed <- map(model_list_untrimmed, ~ trim.values(., trim_values))

model_trimdf <- bind_rows(model_list_trimmed, .id = "Population.Measure") %>%
  separate(Population.Measure, c("Population","Measure"))

removed_data <- model_trimdf %>% 
  group_by(Population, Measure) %>% 
  summarise(removed = unique(removed), original = unique(original)) %>% 
  mutate(str_c(percent = format(round(removed/original*100, digits = 2), nsmall = 2), "%"))

model_unmatched <- split(model_trimdf, list(model_trimdf$Measure))

model_matched <- map(model_unmatched, match.samples)

model_matcheddf <- bind_rows(model_matched, .id = "Measure") %>% 
  mutate(NumPartos = case_when(Sex == "Male" ~ 0, TRUE ~ NumPartos))

model_list <- split(model_matcheddf, list(model_matcheddf$Population, model_matcheddf$Measure))

# t1 <- model_list[["USA.WBC"]]
# t1_model <- gam(value ~ s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase), 
#                 family = gaussian(), data = t1, method = 'REML')
# t2_model <- model_outputs[["USA.WBC"]]

model_outputs <- map(model_list, run.models)
