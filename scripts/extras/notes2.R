

#Extract predicted values, using test as template
#4 models in each list element

M1.age <- sort(unique(M1[M1$Repcat =="Pre-pubertal",]$Age))
M1.

pooled.age <- M1 %>% 
  group_by(Age,Repcat) %>% 
  summarise(Age = get.dezy(Age))

pooled.bmi <- M1 %>% 
  group_by(Sex,Repcat) %>% 
  summarise(BMI = median(BMI))

subset1 <- full_join(pooled.age, pooled.bmi)

rephase.bmi <- M1 %>% 
  filter(Repcat == "Reproductive") %>% 
  group_by(Repcat2) %>% 
  summarise(BMI = median(BMI)) %>% 
  mutate(Repcat = "Reproductive")

subset2 <- na.omit(full_join(rephase.bmi, pooled.age))

subset1
subset2

#M1 = pre-pubertal, M2 = reproductive, M3 = reproductive by phase, M4 = post-reproductive

#Each element = list of models for a single population + measure
#names(test[[1]])

#Function to gather predicted values
get.preds <- function(x){
  labelz <- names(x)[[1]] #Four names 
  Population = str_extract(labelz, "[^.]+")
  Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz)
  
  M1 <- x[[1]]
  M2A <- x[[2]]
  M2B <- x[[3]]
  M3 <- x[[4]]
  
  M1.ss <- subset1 %>% filter(Repcat == "Pre-pubertal") %>% select(Age,BMI,Sex)
  M2A.ss <-subset1 %>% filter(Repcat == "Reproductive") %>% select(Age,BMI,Sex)
  M2B.ss <- subset2 %>% select(Age,BMI,Repcat2)
  M3.ss <-subset1 %>% filter(Repcat == "Post-Reproductive") %>% select(Age,BMI,Sex)
  
  M1.preds <- as_tibble(fitted(M1, newdata = M1.ss, 
                               re_formula = NA, summary = FALSE, ndraws = 100))
  M2A.preds <- as_tibble(fitted(M2A, newdata = M2A.ss, 
                                re_formula = NA, summary = FALSE, ndraws = 100))
  M2B.preds <- as_tibble(fitted(M2B, newdata = M2B.ss, 
                                re_formula = NA, summary = FALSE, ndraws = 100)) %>% 
    mutate(Population = str_extract(labelz, "[^.]+"), 
           Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz))
  M3.preds <- as_tibble(fitted(M3, newdata = M3.ss, 
                               re_formula = NA, summary = FALSE, ndraws = 100))
  
  colnames(M1.preds) <- c("Male","Female")
  colnames(M2A.preds) <- c("Male","Female")
  colnames(M3.preds) <- c("Male","Female")
  colnames(M2B.preds) <- c("Cycling","Postpartum","Pregnant","Male",
                           str_extract(labelz, "[^.]+"),
                           gsub(".*\\.(.*)\\..*", "\\1", labelz))
  
  preds <- list(M1.preds, M2A.preds, M3.preds)
  names(preds) <- c("Pre-pubertal","Reproductive","Post-Reproductive")
  age.models <- plyr::ldply(preds, data.frame, .id="Repcat") %>% 
    mutate(Population = str_extract(labelz, "[^.]+"), 
           Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz))
  preds.full <- list(age.models, M2B.preds)
  preds.full  
}

test.preds <- map(test, get.preds)

m1.preds <- plyr::ldply(map(test.preds, function(x) x[[1]]),data.frame) %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Value")
m2.preds <- plyr::ldply(map(test.preds, function(x) x[[2]]),data.frame)




plyr::ldply(matched.list,data.frame, .id="Category")  %>% 
  mutate(Repcat = gsub("\\..*", "", Category),
         Repcat2 = str_c(Sex, RepGroup))


