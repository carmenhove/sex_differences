
## Plot of effects of parity on WBC, NEU, NLR, MON in USA
## Plot of effects of parity 


## Function to gather predicted values from models
get.predictions.parity <- function(x, df){
  
  if(as.character(x$formula[[3]])[[3]] == "s(id, bs = \"re\")"){
    which_population = "Tsimane"
  }else{
    which_population = "USA"
  }
  
  new_data <- df %>%
    filter(Sex != "Male") %>% 
    select(RepPhase, NumPartos, Population) %>% 
    mutate(RepGroup = case_when(RepPhase %in% c("Cycling","Pregnant","Postpartum") ~ "Reproductive", 
                                TRUE ~ RepPhase),
           Age = case_when(RepPhase == "Pre-Menst" ~ 10,
                           RepGroup == "Reproductive" ~ 24,
                           RepPhase == "Menopause" ~ 65),
           BMI_zscore = 0,
           id = 50) %>%
    group_by(Population, RepPhase) %>% 
    distinct(NumPartos, .keep_all = T) %>% 
    select(RepPhase, RepGroup, id, BMI_zscore, Age, NumPartos, Population) %>% 
    filter(Population == which_population)
  
  ilink <- family(x)$linkinv
  set.seed(1234)
  pred <- predict(x, new_data, type = "link", se.fit = TRUE, exclude = "s(id)")
  pred <- cbind(pred, new_data)
  pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),
                     upr_ci = ilink(fit + (2 * se.fit)),
                     fitted = ilink(fit)) 
  pred <- pred  %>%
    mutate(RepPhase = ordered(RepPhase, 
                              levels = c("Pre-Menst",
                                         "Cycling","Pregnant",
                                         "Postpartum",
                                         "Menopause")))
  
  #pred <- full_join(pred1, pred2)
  pred
}

parity_list <- flatten(lapply(model_outputs, function(x) x[-1]))

parity_pred_lists <- map(parity_list, ~get.predictions.parity(., model_matcheddf))

parity_pred_df <- bind_rows(parity_pred_lists, .id = "Population.Measure") %>%
  separate(Population.Measure, c("Population","Measure")) %>%
  mutate(Unit = case_when(Measure !='NLR' ~ "Cells/µL", TRUE ~ "Ratio"))

ggplot(parity_pred_df, aes(x = NumPartos, y = fit, color = RepPhase, fill = RepPhase)) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.3, color = NA) +
  geom_line() +
  facet_grid(Measure ~ Population, scales = "free")

ggplot(model_matcheddf %>% 
         filter(Measure == "WBC"), 
       aes(x = Age, y = NumPartos)) + 
  geom_point(position = "jitter") + 
  facet_grid(~ Population, scales = "free")

