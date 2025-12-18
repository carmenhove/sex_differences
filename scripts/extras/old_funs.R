
#OLD FUNCTIONS

## Function to get ALTERNATIVE Figure 2 plots
get.plot.lifespan.alt <- function(x, group){
  
  if(unique(x$Unit) == "Cells/µL"){
    x <- x %>% mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON")))
  } else {
    x <- x
  }
  
  if(group == "Pre-Menst"){
    color_values <- c("#181818","#F28023")
  }else if(group == "Reproductive"){
    color_values <- c("#181818","#5A870A", "#009BB0", "#A99CD9")
  } else {
    color_values = c("#181818","#B4450E")
  }
  
  male_ranges <- x %>% filter(RepGroup == group) %>%  #"Repro") %>%
    group_by(Measure) %>%
    summarise(male_min = unique(male_min),
              male_max = unique(male_max))
  
  plot2 <- ggplot(x %>% filter(RepGroup == group), #"Repro"), 
                  aes(x = RepPhase, fill = RepPhase, color =RepPhase)) + 
    geom_boxplot(aes(
      lower = lwr_ci,
      upper = upr_ci,
      middle = fit,
      ymin = fit - 3*se.fit,
      ymax = fit + 3*se.fit),
      stat = "identity",
      width = 0.5,
      alpha = 0.3) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) + 
    geom_rect(data=male_ranges, inherit.aes=FALSE,
              aes(ymin=male_min, ymax=male_max,
                  xmin=-Inf, xmax=Inf,
                  group=Measure), alpha=0.15,
              color = NA, fill = "#181818") +
    facet_grid(Measure ~ Population, scales = "free") + 
    scale_fill_manual(values = color_values) + #c("#181818","#5A870A", "#009BB0", "#A99CD9"))+
    scale_color_manual(values = color_values) + #c("#181818","#5A870A", "#009BB0", "#A99CD9"))+
    guides(fill=guide_legend(title="Sex:Reproductive \nPhase"),
           color =guide_legend(title="Sex:Reproductive \nPhase"))+
    ylab(unique(x$Unit))
  
  plot2
}

## Function to get Figure 1 plots
get.plot.lifespan <- function(x, z){
  
  z2 <- z %>% 
    filter(Population == unique(x$Population),
           Unit == unique(x$Unit))
  
  if(unique(x$Unit) == "Cells/µL"){
    x <- x %>% mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON")))
    z2 <- z2 %>% mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON")))
  } else {
    x <- x
    z2 <- z2
  }
  
  plot1 <- ggplot(x,
                  aes(x = Age, y = fit, color = Sex, fill = Sex)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci),
                alpha = 0.3, color = NA) +
    geom_line() +
    geom_rect(data=z2, inherit.aes=FALSE,
              aes(xmin=to, xmax=from,
                  ymin=ymin, ymax=ymax,
                  group=Group, fill = bias), color = NA, alpha = 0.15) +
    labs(x = "Age", y = unique(x$Unit)) + 
    facet_grid(Measure ~ Population, scales = "free") +
    scale_fill_manual(values = c("#B4450E","#181818"))+
    scale_color_manual(values = c("#B4450E","#181818"))
  plot1
}

## Function to get ALTERNATIVE Figure 2 plots
get.plot.lifespan.alt <- function(x, group){
  
  if(unique(x$Unit) == "Cells/µL"){
    x <- x %>% mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON")))
  } else {
    x <- x
  }
  
  if(group == "Pre-Menst"){
    color_values <- c("#181818","#F28023")
  }else if(group == "Reproductive"){
    color_values <- c("#181818","#5A870A", "#009BB0", "#A99CD9")
  } else {
    color_values = c("#181818","#B4450E")
  }
  
  male_ranges <- x %>% filter(RepGroup == group) %>%  #"Repro") %>%
    group_by(Measure) %>%
    summarise(male_min = unique(male_min),
              male_max = unique(male_max))
  
  plot2 <- ggplot(x %>% filter(RepGroup == group), #"Repro"), 
                  aes(x = RepPhase, fill = RepPhase, color =RepPhase)) + 
    geom_boxplot(aes(
      lower = lwr_ci,
      upper = upr_ci,
      middle = fit,
      ymin = fit - 3*se.fit,
      ymax = fit + 3*se.fit),
      stat = "identity",
      width = 0.5,
      alpha = 0.3) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) + 
    geom_rect(data=male_ranges, inherit.aes=FALSE,
              aes(ymin=male_min, ymax=male_max,
                  xmin=-Inf, xmax=Inf,
                  group=Measure), alpha=0.15,
              color = NA, fill = "#181818") +
    facet_grid(Measure ~ Population, scales = "free") + 
    scale_fill_manual(values = color_values) + #c("#181818","#5A870A", "#009BB0", "#A99CD9"))+
    scale_color_manual(values = color_values) + #c("#181818","#5A870A", "#009BB0", "#A99CD9"))+
    guides(fill=guide_legend(title="Sex:Reproductive \nPhase"),
           color =guide_legend(title="Sex:Reproductive \nPhase"))+
    ylab(unique(x$Unit))
  
  plot2
}

## Function to run appropriate models
run.models <- function(x){
  #Specify model formulas
  if(unique(x$Population) == "Tsimane" & unique(x$measure) != "NLR"){
    model1_formula <- as.formula("as.integer(value) ~ s(Age, by = Sex) + BMI_zscore + s(id, bs = 're')")
    model2_formula <- as.formula("as.integer(value) ~ RepPhase + s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase) + s(id, bs = 're')")
  } else if(unique(x$Population) == "Tsimane" & unique(x$measure) == "NLR"){
    model1_formula <- as.formula("value ~ s(Age, by = Sex) + BMI_zscore + s(id, bs = 're')")
    model2_formula <- as.formula("value ~ RepPhase + s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase) + s(id, bs = 're')")
  } else if(unique(x$Population)=="USA" & unique(x$measure)=="NLR"){
    model1_formula <- as.formula("value ~ s(Age, by = Sex) + BMI_zscore")
    model2_formula <- as.formula("value ~ RepPhase + s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase)")
  } else {
    model1_formula <- as.formula("as.integer(value) ~ s(Age, by = Sex) + BMI_zscore")
    model2_formula <- as.formula("as.integer(value) ~ RepPhase + s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase)")
  }
  # #Specify model families
  ifelse(str_c(unique(x$measure),".", unique(x$Population)) == "BAS.Tsimane", 
         model_family <- "gaussian()", #ziP()", 
         model_family <- "gaussian()")
  #Model 1
  set.seed(1234)
  model1 <- gam(model1_formula, family = model_family, data = x, method = 'REML')
  #Model 2
  x2 <- x %>% filter(Sex == "Female")
  set.seed(1234)
  model2 <- gam(model2_formula, family = model_family, data = x2, method = 'REML')
  #Combine
  models <- list(model1, model2)
  #Print
  models
}

## Function to gather predicted values from models
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
  
  df1 <- df %>%
    filter(Sex != "Male") %>% 
    select(RepPhase, Age) %>% 
    mutate(RepGroup = case_when(RepPhase %in% c("Cycling","Pregnant","Postpartum") ~ "Reproductive", TRUE ~ RepPhase),
           BMI_zscore = 0, id = 50) %>% 
    group_by(RepPhase) %>% 
    distinct(Age, .keep_all = T)
  
  new_data2 <- data.frame(NumPartos=0:5) %>% 
    merge(df1, by=NULL) %>% 
    filter(!(RepPhase %in% c("Male","Pre-Menst") & NumPartos > 0), 
           !(RepPhase == "Postpartum" & NumPartos == 0)) %>% 
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





#PREDICTED VALUES

## Can now set parity to whichever number you want to highlight effects of parity on NLR in the USA
pred_lists <- map(model_outputs, ~ get.predictions(., model_matcheddf))

pred_df <- bind_rows(pred_lists, .id = "Population.Measure") %>%
  separate(Population.Measure, c("Population","Measure")) %>%
  mutate(model = case_when(!is.na(Sex) ~ "model 1", TRUE ~ "model 2"),
         RepPhase = case_when(model == "model 1" & Sex == "Male" ~ "Male", TRUE ~ RepPhase),
         Unit = case_when(Measure !='NLR' ~ "Cells/µL", TRUE ~ "Ratio")) %>% 
  group_by(Age, Population, Measure, model) %>%
  mutate(exp_fit = exp(fit),
         exp_upr_ci = exp(upr_ci),
         exp_lwr_ci = exp(lwr_ci))

## PRIMARY FIGURES
model_1df <- pred_df %>% 
  filter(model == "model 1") %>%
  group_by(Age, Population, Measure, model) %>%
  mutate(Bias = case_when(lwr_ci[Sex == "Male"] > upr_ci[Sex == "Female"] ~ "Male",
                          lwr_ci[Sex == "Female"] > upr_ci[Sex == "Male"] ~ "Female",
                          TRUE ~ "No Bias")) %>% 
  arrange(Population, Measure, Age) %>%
  group_by(Population, Measure) %>% 
  mutate(Group = cumsum(Bias != lag(Bias, default = Bias[[1]])) + 1) %>% 
  ungroup() %>%
  mutate(Unit = case_when(Measure !='NLR' ~ "Cells/µL", TRUE ~ "Ratio"))


## Get model predictions from value ~ reproductive phase models (model 2s), plus male-specific data from model 1
model_2s <- pred_df %>% 
  filter(model == "model 2") %>% 
  mutate(Sex = "Female") %>% 
  full_join(., pred_df %>% filter(model == "model 1", Sex == "Male")) %>% 
  group_by(Age, Population, Measure) %>%
  mutate(fit = case_when(Unit == "Ratio" ~ round(fit, digits = 2), TRUE ~ round(fit)),
         lwr_ci = case_when(Unit == "Ratio" ~ round(lwr_ci, digits = 2), TRUE ~ round(lwr_ci)),
         upr_ci = case_when(Unit == "Ratio" ~ round(upr_ci, digits = 2), TRUE ~ round(upr_ci)),
         Male_median = fit[Sex == "Male"],
         Male_lwr = lwr_ci[Sex == "Male"], 
         Male_upr = upr_ci[Sex == "Male"],
         NumPartos = case_when(Sex == "Male" ~ 0,
                               TRUE ~ NumPartos))

model_2list <- split(model_2s, list(model_2s$NumPartos))

model_2df <- map(model_2list, get.dfs) %>% 
  bind_rows(., .id = "NumPartos")

model_matcheddf %>% 
  group_by(Population, RepPhase) %>% 
  summarise(min_par = min(NumPartos), 
            par_25 = quantile(NumPartos, na.rm = T, probs = 0.25),
            par_75 = quantile(NumPartos, na.rm = T, probs = 0.75),
            max_par = max(NumPartos), 
            median_par = median(NumPartos))

## Effects of parity
parity_list <- flatten(lapply(model_outputs, function(x) x[-1]))

parity_pred_lists <- map(parity_list, ~get.predictions.parity(., model_matcheddf))

parity_pred_df <- bind_rows(parity_pred_lists, .id = "Population.Measure") %>%
  separate(Population.Measure, c("Population","Measure")) %>%
  mutate(Unit = case_when(Measure !='NLR' ~ "Cells/µL", TRUE ~ "Ratio"))

ggplot(parity_pred_df, aes(x = NumPartos, y = fit, color = RepPhase, fill = RepPhase)) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.3, color = NA) +
  geom_line() +
  facet_grid(Measure ~ Population, scales = "free")
# 
ggplot(model_matcheddf %>%
         filter(Measure == "WBC"),
       aes(x = Age, y = NumPartos)) +
  geom_point(position = "jitter") +
  facet_grid(~ Population, scales = "free")

