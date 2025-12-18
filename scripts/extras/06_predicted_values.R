pooled.bmi <- M1 %>% 
  group_by(Sex,Repcat) %>% #
  summarise(BMI = median(BMI)) %>% 
  mutate(Repcat = ordered(Repcat, levels = c("Pre-pubertal",
                                             "Reproductive",
                                             "Post-Reproductive"))) %>% 
  arrange(Sex,Repcat)

m1.age <- sort(unique(M1[M1$Repcat == "Pre-pubertal",]$Age))
m2.age <- sort(unique(M1[M1$Repcat == "Reproductive",]$Age))
m3.age <- sort(unique(M1[M1$Repcat == "Post-Reproductive",]$Age))

m1.length <- length(m1.age)
m2.length <- length(m2.age)
m3.length <- length(m3.age)
length.age <- length(c(m1.age,m2.age,m3.age))

m1.df <- data.frame(Age = rep(m1.age,each=2),
                    Sex=rep(c("Male","Female"),times=m1.length),
                    BMI = rep(c(pooled.bmi[[1,3]],pooled.bmi[[4,3]]),
                              times = m1.length),
                    Repcat = "Prepubertal") %>% 
  mutate(Colnamz = str_c(Sex,Age,round(BMI),Repcat,sep = "."))
                            
m2.df <- data.frame(Age = rep(m2.age,each=2),
                    Sex=rep(c("Male","Female"),times=m2.length),
                    BMI = rep(c(pooled.bmi[[2,3]],pooled.bmi[[5,3]]),
                              times = m2.length),
                    Repcat = "Reproductive") %>% 
  mutate(Colnamz = str_c(Sex,Age,round(BMI),Repcat,sep = "."))

m2B.df <- data.frame(Age = rep(m2.age,each=4),
                              Repcat2=rep(c("MaleMale","FemaleCycling",
                                        "FemalePregnant","FemalePostpartum"),
                                      times=m2.length),
                              BMI = rep(c(pooled.bmi[[2,3]],
                                          rep(pooled.bmi[[5,3]],times=3)),
                                        times = m2.length)) %>% 
  mutate(Colnamz = str_c(Age,round(BMI),Repcat2,sep = "."))

                  
m3.df <- data.frame(Age = rep(m3.age,each=2),
                    Sex=rep(c("Male","Female"),times=m3.length),
                    BMI = rep(c(pooled.bmi[[3,3]],pooled.bmi[[6,3]]),
                              times = m3.length),
                    Repcat = "PostReproductive") %>% 
  mutate(Colnamz = str_c(Sex,Age,round(BMI),Repcat,sep = "."))


#Function to gather predicted values
get.fits <- function(x){
  labelz <- names(x)[[1]] #Four names 
  Population = str_extract(labelz, "[^.]+")
  Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz)
  
  M1 <- x[[1]]
  M2A <- x[[2]]
  M2B <- x[[3]]
  M3 <- x[[4]]
  
  M1.ss <- m1.df
  M2A.ss <-m2.df
  M2B.ss <- m2B.df
  M3.ss <-m3.df

    M1.preds <- as_tibble(fitted(M1, newdata = M1.ss, 
                                 re_formula = NA, summary = FALSE, ndraws = 100))
    M2A.preds <- as_tibble(fitted(M2A, newdata = M2A.ss, 
                                  re_formula = NA, summary = FALSE, ndraws = 100))
    M2B.preds <- as_tibble(fitted(M2B, newdata = M2B.ss, 
                                  re_formula = NA, summary = FALSE, ndraws = 100))
    M3.preds <- as_tibble(fitted(M3, newdata = M3.ss, 
                                 re_formula = NA, summary = FALSE, ndraws = 100))

  colnames(M1.preds) <- m1.df$Colnamz
  colnames(M2A.preds) <- m2.df$Colnamz
  colnames(M3.preds) <- m3.df$Colnamz
  colnames(M2B.preds) <- m2B.df$Colnamz
  
  preds <- list(M1.preds, M2A.preds, M3.preds)
  age.models <- plyr::ldply(preds, data.frame) %>% 
    mutate(Population = str_extract(labelz, "[^.]+"), 
          Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz))
  phase.models <- M2B.preds %>% 
    mutate(Population = str_extract(labelz, "[^.]+"), 
           Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz))

  preds.full <- list(age.models, phase.models)
  preds.full 
  }

#GET POSTERIOR DRAWS
#Function to gather predicted values
get.posts <- function(x){
  labelz <- names(x)[[1]] #Four names 
  Population = str_extract(labelz, "[^.]+")
  Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz)
  
  M1 <- x[[1]]
  M2A <- x[[2]]
  M2B <- x[[3]]
  M3 <- x[[4]]
  
  M1.ss <- m1.df
  M2A.ss <-m2.df
  M2B.ss <- m2B.df
  M3.ss <-m3.df
  
  M1.preds <- as_tibble(posterior_predict(M1, newdata = M1.ss, 
                                          re_formula = NA, summary = FALSE, ndraws = 100))
  M2A.preds <- as_tibble(posterior_predict(M2A, newdata = M2A.ss, 
                                           re_formula = NA, summary = FALSE, ndraws = 100))
  M2B.preds <- as_tibble(posterior_predict(M2B, newdata = M2B.ss, 
                                           re_formula = NA, summary = FALSE, ndraws = 100))
  M3.preds <- as_tibble(posterior_predict(M3, newdata = M3.ss, 
                                            re_formula = NA, summary = FALSE, ndraws = 100))
  
  colnames(M1.preds) <- m1.df$Colnamz
  colnames(M2A.preds) <- m2.df$Colnamz
  colnames(M3.preds) <- m3.df$Colnamz
  colnames(M2B.preds) <- m2B.df$Colnamz
  
  preds <- list(M1.preds, M2A.preds, M3.preds)
  age.models <- plyr::ldply(preds, data.frame) %>% 
    mutate(Population = str_extract(labelz, "[^.]+"), 
           Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz))
  phase.models <- M2B.preds %>% 
    mutate(Population = str_extract(labelz, "[^.]+"), 
           Measure = gsub(".*\\.(.*)\\..*", "\\1", labelz))
  
  preds.full <- list(age.models, phase.models)
  preds.full 
}

fits.models <- model.list[c("NEU.THLHP","NEU.NHANES",
                            "WBC.THLHP","WBC.NHANES",
                            "NLR.THLHP","NLR.NHANES",
                            "EOS.THLHP","MON.NHANES")]
preds.fits <- map(fits.models, get.fits)

posts.models <- model.list[c("BAS.THLHP","BAS.NHANES",
                             "MON.THLHP","EOS.NHANES")]
preds.posts <- map(posts.models, get.posts)

test.values <- c(preds.fits, preds.posts)

pred.values <- c(preds.fits,preds.posts)
#mon.preds <- map(model.list["MON.THLHP"],get.preds)

age.limits <- M1 %>% 
  group_by(Population,Repcat) %>% 
  summarise(MinAge = min(Age),
            MaxAge = max(Age))

m1.preds <- na.omit(plyr::ldply(map(test.values, function(x) x[[1]]),data.frame) %>% 
  pivot_longer(Male.0.17.Prepubertal:Female.91.28.PostReproductive, 
               names_to = "Sex.Age.BMI.Repcat",values_to = "Value") %>% 
  separate(Sex.Age.BMI.Repcat, c("Sex","Age","BMI","Repcat")) %>% 
  mutate(Age = as.numeric(Age),
         BMI = as.numeric(BMI))) %>% 
  filter((Population == "NHANES" & Repcat == "Prepubertal" & Age >=2 & Age <=13)|
           (Population == "THLHP" & Repcat == "Prepubertal" & Age >= 0 & Age <=17)|
           (Population =="NHANES" & Repcat == "Reproductive" & Age >= 12 & Age <=52)|
           (Population == "THLHP" & Repcat == "Reproductive" & Age >=9 & Age <=54)|
           (Population == "NHANES" & Repcat == "PostReproductive" & Age >= 39 & Age <= 85)|
           (Population == "THLHP" & Repcat == "PostReproductive" & Age >=40 & Age <=91))


m2.preds <- na.omit(plyr::ldply(map(test.values, function(x) x[[2]]),data.frame) %>% 
  pivot_longer(X9.24.MaleMale:X54.25.FemalePostpartum, 
               names_to = "Age.BMI.Repcat2",values_to = "Value") %>% 
  separate(Age.BMI.Repcat2, c("Age","BMI","Repcat2")) %>% 
  mutate(Age = as.numeric(str_extract(Age, "[^X]+")),
         BMI = as.numeric(BMI))) %>% 
  filter((Population =="NHANES" & Age >= 12 & Age <=52)|
           (Population == "THLHP" & Age >=9 & Age <=54))


