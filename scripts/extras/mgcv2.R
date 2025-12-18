
MERGED.F <- MERGED %>% 
  mutate(RepGroupAdj = case_when(RepGroup == "Male" ~ "Male",
                                 RepGroup %in% c("Pre-menarche",
                                                 "Cycling","Menopause") ~ "NP/NPP Female",
                                 RepGroup == "Pregnant" ~ "Pregnant Female",
                                 RepGroup == "Postpartum" ~ "Postpartum"),
         RepGroupAdj = as.factor(RepGroupAdj)) %>% 
  pivot_longer(WBC:NLR, names_to = "Measure", values_to = "Value") 

split.MERGED <- split(MERGED.F, list(MERGED.F$Measure, MERGED.F$Population))

#b0 <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),
#          family=negbin(3),data=dat)

install.packages("gratia")  
library(gratia)

names(split.MERGED)
test <- mgcv::gam(log(Value) ~ s(Age, by = RepGroupAdj)+BMI+(1|pid), 
                  data = split.MERGED[[18]], method = 'REML')

range(MERGED.F[MERGED.F$RepGroupAdj=="Male",]$Age)
range(MERGED.F[MERGED.F$RepGroupAdj=="NP/NPP Female",]$Age)
range(MERGED.F[MERGED.F$RepGroupAdj=="Pregnant Female",]$Age)
range(MERGED.F[MERGED.F$RepGroupAdj=="Postpartum",]$Age)

new_data <- data.frame(Age = c(seq(0,90),seq(0,91),seq(13,54),seq(12,54)),
                       RepGroupAdj = c("Male","NP/NPP Female",
                                       "Pregnant Female","Postpartum"),
                       

new_data <- with(test2, expand.grid(Age = seq(min(Age),max(Age),length=200),
                                    BMI = median(BMI)))
ilink <- family(test)$linkinv
preds <- predict(test, new_data, type = "response", se.fit = T)
                # values = list(BMI = median(test2$BMI,na.rm = T)))

test2$preds <- preds

ggplot(test2, aes(x = Age, y = preds, colour = RepGroup)) +
  #geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, 
  #                colour = NULL, fill = RepGroup),
  #            alpha = 0.5) +
  geom_line() +
  labs(title = "NHANES")+
  ylab("lnWBC")#+


sm <- gratia::smooth_estimates(test, data = test2) %>%
  add_confint()

ggplot(preds, aes(x = Age, y = est, colour = RepGroup)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, 
                  colour = NULL, fill = RepGroup),
              alpha = 0.5) +
  geom_line() +
  labs(title = "NHANES")+
  ylab("lnWBC")#+
  #facet_wrap(~ RepGroup)
