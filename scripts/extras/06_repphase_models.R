
test <- gam(value ~ s(Age, by = RepPhase),
            data = model_list[["USA.WBC"]], 
            method = 'REML')

summary(test)

new_data <- #model_list[["USA.WBC"]] %>%
  model_df %>%
  group_by(RepPhase) %>%
  distinct(Age)

ilink <- family(test)$linkinv
pred <- predict(test, new_data, type = "link", se.fit = TRUE)
pred <- cbind(pred, new_data)
pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),
                  upr_ci = ilink(fit + (2 * se.fit)),
                  fitted = ilink(fit)) #%>%
#mutate(Population.Measure = z) %>%
#separate(Population.Measure, c("Population","Measure"))
pred

range(pred[pred$RepPhase %in% c("Cycling","Pregnant","Postpartum"),]$Age)
median(model_df[model_df$RepPhase %in% c("Cycling","Pregnant","Postpartum"),]$Age)

pred2 <- pred %>% filter(RepPhase %in% c("Cycling","Pregnant","Postpartum") |
                           (RepPhase == "Male" & Age >= 12 & Age <=66))

ggplot(pred, aes(x = Age, y = fitted, color = RepPhase)) + 
  geom_point()

plot2 <- ggplot(pred2, aes(x = Age, y = fitted, color = RepPhase, fill = RepPhase)) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2, color = NA) +
  geom_line() + 
  geom_vline(xintercept = 28, color = "darkgrey") + 
  labs(x = "Age", y = "WBC")

library(ggpubr)
ggarrange(plot1, plot2, ncol = 1, heights = c(1.5,1))

pred2 %>% 
  filter(Age == 28) %>% 
  arrange(-fitted) %>% 
  select(RepPhase, Age, fitted, lwr_ci, upr_ci) %>%
  #mutate(RepPhase = ordered(RepPhase, 
  #                          levels = c("Cycling","Pregnant","Postpartum","Male"))) %>%
  mutate(Robust = case_when(upr_ci[RepPhase=="Male"] < lwr_ci ~ "Higher",
                            lwr_ci[RepPhase=="Male"] > upr_ci ~ "Lower",
                            RepPhase=="Male" ~ "--",
                            TRUE ~ "Overlapping"))














pred %>% filter(RepPhase=="Pregnant", Age == median(model_list[["USA.WBC"]]$Age))

new_data2 <- model_list[["USA.WBC"]] %>%
  filter(RepPhase != "Male") %>%
  group_by(RepPhase) %>%
  summarise(Age = median(Age))

new_data3 <- new_data2 %>%
  mutate(RepPhase = "Male")#str_c("Male_", RepPhase))

newdata4 <- full_join(new_data2, new_data3)

ilink <- family(test)$linkinv
pred <- predict(test, newdata4, type = "link", se.fit = TRUE)
pred <- cbind(pred, newdata4)
pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),
                  upr_ci = ilink(fit + (2 * se.fit)),
                  fitted = ilink(fit)) #%>%
#mutate(Population.Measure = z) %>%
#separate(Population.Measure, c("Population","Measure"))
pred %>% 
  arrange(Age) %>%
  mutate(value = str_c(round(fitted), " (", round(lwr_ci), ",", round(upr_ci), ")")) %>%
  select(RepPhase, Age, value)


ggplot(pred, aes(x = Age, y = fitted, color = RepPhase)) + 
  geom_point()



# GET MATCHED DATASETS FOR REPRODUCTIVE PHASE MODELS

get.matched <- function(x){
  
  premenst <- x %>% filter(RepPhase %in% c("Pre-Menst","Male"))
  reproductive <- x %>% filter(RepPhase %in% c("Cycling","Pregnant","Postpartum","Male"))
  #pregnant <- x %>% filter(RepPhase %in% c("Pregnant","Male"))
  #postpartum <- x %>% filter(RepPhase %in% c("Postpartum","Male"))
  menopause <- x %>% filter(RepPhase %in% c("Menopause","Male"))
  
  premenst.out <- matchit(Sex ~ Age, data = premenst, 
                          method = "nearest", distance = "euclidean")
  reproductive.out <- matchit(Sex ~ Age, data = reproductive, 
                          method = "nearest", distance = "euclidean")
  #pregnant.out <- matchit(Sex ~ Age, data = pregnant, 
  #                        method = "nearest", distance = "euclidean")
  #postpartum.out <- matchit(Sex ~ Age, data = postpartum, 
  #                        method = "nearest", distance = "euclidean")
  menopause.out <- matchit(Sex ~ Age, data = menopause, 
                          method = "nearest", distance = "euclidean")
  
  out_list <- list(c(premenst.out, cycling.out, pregnant.out, 
                   postpartum.out, menopause.out))
  
  matched_list <- map(out_list, out)
  
  matched
}

matched_list <- map(model_list, get.matched)

#ggplot(matched_list[[1]], aes(x = Age, color = Sex)) + 
#  geom_histogram()

run.repmodels <- function(x){
  m1 <- gam(value ~ RepPhase, 
            data = x, 
            method = 'REML')
  m1
}

matched_outputs <- map(matched_list, run.repmodels)

get.reppredictions <- function(x, df){
  new_data <- df %>%
    group_by(RepPhase) %>%
    distinct(Age)
    
  ilink <- family(x)$linkinv
  pred <- predict(x, new_data, type = "link", se.fit = TRUE)
  pred <- cbind(pred, new_data)
  pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),
                    upr_ci = ilink(fit + (2 * se.fit)),
                    fitted = ilink(fit)) #%>%
  #mutate(Population.Measure = z) %>%
  #separate(Population.Measure, c("Population","Measure"))
  pred
}

#test <- matched_list[[1]] %>%
#  group_by(RepPhase) %>%
#  distinct(Age) %>%
#  summarise(Age.range = str_c(min(Age),"-",max(Age)))

ggplot(test, aes(x = Age, color = RepPhase)) + 
  geom_histogram() + 
  facet_grid(RepPhase ~ ., scales = "free")

pred_list2 <- map(matched_outputs, ~ get.reppredictions(., matched_list[[1]]))

test2 <- get.reppredictions(matched_outputs[[1]], matched_list[[1]])

ggplot(test2, aes(x = Age, y = fitted, color = RepPhase, fill = RepPhase)) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2, color = NA) +
  geom_line() + 
  #labs(x = "Age", y = unique(x$Measure)) + 
  facet_grid(Measure ~ Population, scales = "free")




pred_df2 <- bind_rows(pred_list2, .id = "Population.Measure") %>%
  separate(Population.Measure, c("Population","Measure"))

ggplot(pred_df2, aes(x = Age, y = fitted, color = RepPhase, fill = RepPhase)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2, color = NA) +
    geom_line() + 
    #labs(x = "Age", y = unique(x$Measure)) + 
    facet_grid(Measure ~ Population, scales = "free")

get.plot(pred_df %>% filter(Measure == "NEU"))



