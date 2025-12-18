
ggplot(MERGED,  aes(x = Age, y = log(BMI), color = Sex)) +
  geom_point() + 
  facet_grid(~ Population) + 
  theme(legend.position = "bottom")

ggplot(MERGED,  aes(x = Age, y = log(BMI), color = Sex)) +
  geom_point() + 
  facet_grid(~ Population) + 
  theme(legend.position = "bottom")

#facet my measure
ggplot(MERGED,  aes(x = Age, y = log(WBC), color = Sex)) +
  geom_point() + 
  facet_grid(~ Population) + 
  theme(legend.position = "bottom")


diabetes2 <- full_join(diabetes %>% mutate(SEQN = as.character(SEQN)), 
                       NHdata %>% mutate(SEQN = as.character(SEQN))) %>%
  filter(DID040 < 79) %>%
  mutate(ASD = Age - DID040)

ggplot(diabetes2 %>% filter(!is.na(Sex)),  
       aes(x = DID040, fill = Sex)) +
  geom_density(alpha = 0.5) + 
  xlab("Age at diabetes diagnosis")


ggplot(diabetes2 %>% filter(!is.na(Sex)),  
       aes(x = ASD, fill = Sex)) +
  geom_density(alpha = 0.5) + 
  xlab("Years since diabetes diagnosis")

ggplot(MERGED %>% filter(!is.na(Sex) & Population=="NHANES"),  
       aes(x = Age, y = log(BMI), color = Sex)) +
  geom_point() +
  theme(legend.position = "bottom")

