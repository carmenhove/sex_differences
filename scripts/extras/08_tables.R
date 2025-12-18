


Table1 <- matched.df %>% 
  mutate(Repcat = ordered(Repcat, levels = c("Pre-pubertal",
                          "Reproductive","Post-Reproductive")),
         Sex = ordered(Sex, levels = c("Female","Male"))) %>% 
  group_by(Population,Sex,Repcat) %>% 
  summarise(N = length(pid), 
            RN = length(pid)-length(unique(pid)),
            Age = get.dezies2(Age),
            BMI = get.dezies2(BMI)) %>% 
  arrange(Population,Repcat, Sex)

write.csv(Table1,'Table1.csv')


Table2 <- matched.df %>% 
  filter(Sex == "Female") %>% 
  mutate(Repcat2 = 
           ordered(Repcat2, 
                   levels = c("FemalePre-menarche",
                              "FemaleCycling",
                              "FemalePregnant",
                              "FemalePostpartum",
                              "FemaleMenopause"))) %>% 
  group_by(Population,Repcat2) %>% 
  summarise(N = length(pid), 
            RN = length(pid)-length(unique(pid)),
            Age = get.dezies2(Age),
            BMI = get.dezies2(BMI))

write.csv(Table2,'Table2.csv')



pooled <- full_join(M1 %>% 
  group_by(Sex,Repcat) %>% #
  summarise(BMI = median(BMI)) %>% #Age = median(Age)) %>% 
  mutate(Repcat = ordered(Repcat, levels = c("Pre-pubertal",
                                             "Reproductive",
                                            "Post-Reproductive"))),
  M1 %>% 
    group_by(Repcat) %>% #
    summarise(Age = median(Age)) %>% 
    mutate(Repcat = ordered(Repcat, levels = c("Pre-pubertal",
                                               "Reproductive",
                                               "Post-Reproductive")))
)

#RESULTS TABLES
str(m1.preds)
#m1.table <- m1.preds %>% 
#  filter((Repcat == "Prepubertal" & Age == 7) |
#           (Repcat == "Reproductive" & Age == 28) |
#           (Repcat == "PostReproductive" & Age == 63))

dlist <- split(m1.preds,list(m1.preds$Sex))#m1.table,list(m1.table$Sex)

m1.deltas <- data.frame(Repcat = dlist[[1]]$Repcat,
                         Age = dlist[[1]]$Age,
                         Measure = dlist[[1]]$Measure,
                         Population = dlist[[1]]$Population,
                        Female = dlist[["Female"]]$Value,
                        Male = dlist[["Male"]]$Value,
                        Abs.Delta =  dlist[["Female"]]$Value - dlist[["Male"]]$Value,
                        Pct.Delta = ((dlist[["Female"]]$Value - dlist[["Male"]]$Value) /
                                      dlist[["Female"]]$Value)*100)

Table3 <- m1.deltas %>% 
  group_by(Population, Measure,Repcat) %>% 
  summarise(Female = get.dezies2(Female),
            Male = get.dezies2(Male),
            Abs.Delta = get.dezies2(Abs.Delta),
            Pct.Delta = get.pctdezies2(Pct.Delta)) %>% 
  mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM",
                                               "EOS","MON","BAS",
                                               "NLR")),
         Repcat = ordered(Repcat, levels = c("Prepubertal",
                                             "Reproductive",
                                             "PostReproductive"))) %>% 
  arrange(Measure, Repcat,Population)

write.csv(Table3,'Table3.csv')




str(m2.preds)
#m2.table <- m2.preds %>% 
#  filter(Age == 28)

d2list <- split(m2.preds, list(m2.preds$Repcat2))#m2.table, list(m2.table$Repcat2)) #


m2.deltas <- data.frame(#Repcat2 = d2list[[4]]$Repcat2,
                        Age = d2list[[4]]$Age,
                        Measure = d2list[[4]]$Measure,
                        Population = d2list[[4]]$Population,
                        Abs.Delta.Cyc = d2list[["FemaleCycling"]]$Value - 
                          d2list[["MaleMale"]]$Value,
                        
                        Abs.Delta.Preg = d2list[["FemalePregnant"]]$Value -
                          d2list[["MaleMale"]]$Value,
                        
                        Abs.Delta.PP = d2list[["FemalePostpartum"]]$Value -
                          d2list[["MaleMale"]]$Value) %>% 
  mutate(Pct.Delta.Cyc = (Abs.Delta.Cyc /d2list[["FemaleCycling"]]$Value)*100, 
                        #((d2list[["FemaleCycling"]]$Value -
                                         #   d2list[["FemaleCycling"]]$Value) /
         Pct.Delta.Preg = (Abs.Delta.Preg /d2list[["FemalePregnant"]]$Value)*100,
                        #((d2list[["MaleMale"]]$Value -
                                          #   d2list[["FemalePregnant"]]$Value) /
                                            #d2list[["MaleMale"]]$Value)*100,
         Pct.Delta.PP = (Abs.Delta.PP /d2list[["FemalePostpartum"]]$Value)*100)
                          #((d2list[["MaleMale"]]$Value -
                                       #   d2list[["FemalePostpartum"]]$Value) /
                                       #  d2list[["MaleMale"]]$Value)*100)

Table4 <- m2.deltas %>% 
  group_by(Population, Measure) %>% 
  summarise(Cyc.Delta = get.dezies2(Abs.Delta.Cyc),
            Cyc.PctDelta = get.pctdezies2(Pct.Delta.Cyc),
            
            Preg.Delta = get.dezies2(Abs.Delta.Preg),
            Preg.PctDelta = get.pctdezies2(Pct.Delta.Preg),
            
            PP.Delta = get.dezies2(Abs.Delta.PP),
            PP.PctDelta = get.pctdezies2(Pct.Delta.PP)) %>% 
  mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM",
                                               "EOS","MON","BAS",
                                               "NLR"))) %>% 
  arrange(Measure, Population) #%>%   
  #pivot_longer(Cyc.Delta:PP.PctDelta, names_to = "Type.Repcat", values_to = "Value") %>% 
  #mutate(Type = (gsub("\\..*", "", Type.Repcat))) #%>% 
  #select(-Type.Repcat)# %>% 
  #group_by(Population, Measure) %>% 
  #pivot_wider(Type, names_from = "Type", values_from = "Value")


write.csv(Table4,'Table4.csv')


