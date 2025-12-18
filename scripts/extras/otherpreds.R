#THLHP MONOCYTES AND BASOPHILS
m1.mon.preds <- mon.preds[[1]][[1]] %>% 
  pivot_longer(Male.0.17.Prepubertal:Female.91.28.PostReproductive, 
               names_to = "Sex.Age.BMI.Repcat",values_to = "Value") %>% 
  separate(Sex.Age.BMI.Repcat, c("Sex","Age","BMI","Repcat")) %>% 
  mutate(Age = as.numeric(Age),
         BMI = as.numeric(BMI)) %>% 
  filter(#(Population == "NHANES" & Repcat == "Prepubertal" & Age >=2 & Age <=13)|
    (Population == "THLHP" & Repcat == "Prepubertal" & Age >= 0 & Age <=17)|
      #(Population =="NHANES" & Repcat == "Reproductive" & Age >= 12 & Age <=52)|
      (Population == "THLHP" & Repcat == "Reproductive" & Age >=9 & Age <=54)|
      #(Population == "NHANES" & Repcat == "PostReproductive" & Age >= 39 & Age <= 85)|
      (Population == "THLHP" & Repcat == "PostReproductive" & Age >=40 & Age <=91))

m2.mon.preds <- mon.preds[[1]][[2]] %>% 
  pivot_longer(`9.24.MaleMale`:`54.25.FemalePostpartum`, 
               names_to = "Age.BMI.Repcat2",values_to = "Value") %>% 
  separate(Age.BMI.Repcat2, c("Age","BMI","Repcat2")) %>% 
  mutate(Age = as.numeric(str_extract(Age, "[^X]+")),
         BMI = as.numeric(BMI)) %>% 
  filter((Population =="NHANES" & Age >= 12 & Age <=52)|
           (Population == "THLHP" & Age >=9 & Age <=54))
