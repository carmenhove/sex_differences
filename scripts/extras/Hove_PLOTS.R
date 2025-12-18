
#CYC-Preg-PP group
ggplot(MERGED, 
       aes(y = log(WBC), x = Group, fill = Population)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log(NEU), color = Population), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  #facet_grid(~ Population) + 
  #guides(fill = FALSE) +
  #guides(color = FALSE) +
  #scale_color_brewer(palette = "Spectral") +
  #scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme

#WBC by age and repphase
ggplot(MERGED, aes(x = Age, y = log(WBC),  color = RepPhase)) +  
  geom_point(size=2, position="jitter") + 
  scale_shape_manual(values = c(0, 19)) + 
  facet_grid(Sex ~ Population, scales = "fixed") 

ggplot(MERGED, aes(x = Age, y = log(WBC),  color = Sex)) +  
  geom_point(size=2, position="jitter") + 
  scale_shape_manual(values = c(0, 19)) + 
  facet_grid(Population ~ Sex, scales = "fixed")

ggplot(MERGED, aes(x = Age, y = lnEstradiol,  color = Sex)) +  
  geom_point(size=2, position="jitter") + 
  scale_shape_manual(values = c(0, 19)) + 
  facet_grid(Population ~ ., scales = "fixed")

ggplot(MERGED, aes(x = Age, y = lnTestosterone,  color = Population)) +  
  geom_point(size=2, position="jitter") + 
  scale_shape_manual(values = c(0, 19)) + 
  facet_grid(Population ~ ., scales = "fixed")
