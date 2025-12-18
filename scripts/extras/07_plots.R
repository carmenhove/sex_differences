

#THLHP only: hookworm infection by sex
ggplot(FINAL %>% filter(Population == "THLHP" & !is.na(Hookworm)), 
       aes(x = Category, fill = Sex)) + 
  geom_bar(alpha = 0.7) + 
  facet_grid(~ Hookworm, scales = "free")

#Estradiol ~ Age, by population 
ggplot(MERGED,  aes(x = Age, y = lnEstradiol, color = Sex)) + 
  geom_point() + 
  facet_grid(Population ~., scales = "free")



#CYC-Preg-PP group
ggplot(FINAL,  aes(y = lnWBC, x = Category, fill = Sex)) +
  geom_split_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log(NEU), color = Population), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  facet_grid(Population ~., scales = "free")


ggplot(FINAL, aes(x = lnWBC, fill = Sex)) + 
  geom_density(alpha = 0.7) + 
  facet_grid(Population ~ Category, scales = "free") + 
  coord_flip()
