ggplot(matched.df, aes(x = Age, color = Sex, fill = Sex)) + 
  geom_histogram()+
  facet_grid(Population ~ ., scales = "free")

ggplot(matched.df, aes(x = log(NLR),color = Sex, fill = Sex)) + 
  geom_density(alpha = 0.5)+
  facet_grid(Population ~ Repcat, scales = "free")

ggplot(matched.df, aes(x = log(NLR),color = Repcat2, fill = Repcat2)) + 
  geom_density(alpha = 0.5)+
  facet_grid(Population ~ Repcat, scales = "free")

ggplot(matched.df %>% filter(Population == "NHANES"), 
       aes(x = Age, y = log(BMI), color = Repcat))+
  geom_point(position = "jitter") +
  facet_grid(~ Repcat, scales = "free")




ggplot(matched.df, aes(x = Age, y = log(NLR), color = Repcat2)) + 
  geom_point(position = "jitter") + 
  facet_grid(Population ~ Repcat, scales = "free")

ggplot(MERGED, aes(x = Parity, color = RepGroup)) +
  geom_histogram() + 
  facet_grid(RepGroup ~ Population, scales = "free")

test <- matched.df %>% filter(Repcat != "Post-Reproductive" & 
                                Population == "THLHP")

t1 <- brm(lnNLR ~ Age*Sex+BMI+(1|pid),
          data = test, family = gaussian(),
          iter = 2500, chains=4, cores=2)

wi_prior <- get_prior(lnNLR ~ Age*Sex+BMI,
                      data = test, family = gaussian())
wi_prior$prior[1] <- "normal(0,10)"
test2 <- brm(lnNLR ~ Age*Sex+BMI,
             data = test, family = gaussian(),
             prior = wi_prior,
             iter = 2500, chains=4, cores=2)

test3 <- brm(lnNLR ~ Sex+BMI+Age,
             data = test, family = gaussian(),
             #prior = wi_prior,
             iter = 2500, chains=4, cores=2)
