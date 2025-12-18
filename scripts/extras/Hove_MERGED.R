
MERGED <- full_join(THLHP, remove_all_labels(NHANES)) %>%
  mutate(Population = factor(Population),
         RepPhase = factor(RepPhase),
         Group = factor(Group))

descstats <- MERGED %>%
  select(Population, RepPhase, Measure, pid, Age, BMI, NumPartos) %>%
  group_by(Population, RepPhase, Measure) %>%
  summarise(N = length(pid),
            RN = length(pid)-length(unique(pid)),
            Age = paste0(round(median(Age))," (",round(min(Age)),"-",round(max(Age)),")"),
            BMI = paste0(signif(median(BMI), digits = 4), 
                         " (",signif(min(BMI), digits = 4),
                         "-",signif(max(BMI), digits = 4),")"),
            Parity = paste0(median(NumPartos)," (",min(NumPartos),"-",max(NumPartos),")"),
            NP = paste0(sum(NumPartos==1), " (", signif(sum(NumPartos==1)/N, digits=4)*100,"%)"))#number and % nullparous

write.csv(descstats, "Table2.csv")