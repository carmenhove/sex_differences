#MODEL 1 dataframes
m.THLHP <- matched.df %>% filter(Population == "THLHP") %>% 
  mutate(BAS = round(BAS), MON = round(MON), EOS = round(EOS)) %>% 
  select(Sex:BAS,NLR,pid,Repcat2,Repcat) %>% 
  pivot_longer(WBC:NLR, names_to = "Measure", values_to = "Value") %>% 
  mutate(Family = case_when(Measure %in% c("WBC","NEU","LYM","NLR") ~ "gaussian",
                            #%in% c("WBC","NEU","LYM","crp","NLR","IgE") ~ "gaussian",
                            Measure == "EOS" ~ "negbinomial",
                            Measure %in% c("BAS","MON") ~ "zero_inflated_poisson"),
         Value = case_when(Measure %in% c("WBC","NEU","LYM","NLR") ~ log(Value),
                           Measure %in% c("BAS","EOS","MON") ~ Value))


m.NHANES <- matched.df %>% filter(Population == "NHANES") %>% 
  mutate(BAS = round(BAS), EOS = round(EOS)) %>% 
  select(Sex:BAS,NLR,pid,Repcat2,Repcat) %>% 
  pivot_longer(WBC:NLR, names_to = "Measure", values_to = "Value") %>% 
  mutate(Family = case_when(Measure %in% c("WBC","NEU","LYM","NLR","MON") ~ "gaussian",
                            Measure == "EOS" ~ "negbinomial",
                            Measure == "BAS" ~ "zero_inflated_poisson"),
         Value = case_when(Measure %in% c("WBC","NEU","LYM","NLR") ~log(Value),
                           Measure %in% c("MON","BAS","EOS") ~ Value))

M1 <- full_join(m.THLHP, m.NHANES)

split.M1 <- split(M1, list(M1$Measure, M1$Population))#M1$Repcat, 

split2.M1 <- split(M1, list(M1$Measure, M1$Repcat))

test1 <- split2.M1[["WBC.Reproductive"]] %>% filter(Population == "NHANES")
test2 <- split.M1[["WBC.NHANES"]]

library(mgcv)
library(tidymv)
test <- mgcv::gam(Value ~ s(Age, by = RepGroup)+BMI,
          #k =length(knot.values),
          #knots = list(x = knot.values),
          data = test2)

test.pred <- predict_gam(test,
                         values = list(Age = unique(test2$Age),
                            BMI = median(test2$BMI),
                            RepGroup = unique(test2$RepGroup)))

ggplot(test.pred, aes(x = Age, y = fit, 
                          color =RepGroup, fill = RepGroup))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(ymin = fit-se.fit, 
                  ymax =fit+se.fit, group = RepGroup),
              alpha=0.6, linetype = 0)
