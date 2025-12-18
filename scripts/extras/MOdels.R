library(cmdstanr)
check_cmdstan_toolchain()
cmdstan_path()
cmdstan_version()
options(brms.backend = "cmdstanr")

#MODEL 1 dataframes
m.THLHP <- matched.df %>% filter(Population == "THLHP") %>% 
  mutate(BAS = round(BAS), MON = round(MON), EOS = round(EOS)) %>% 
  select(Sex:BAS,NLR,pid,Repcat2,Repcat) %>% 
  pivot_longer(WBC:NLR, names_to = "Measure", values_to = "Value") %>% 
  mutate(Family = case_when(Measure %in% c("WBC","NEU","LYM","NLR") ~ "gaussian",
                            #%in% c("WBC","NEU","LYM","crp","NLR","IgE") ~ "gaussian",
                            Measure == "EOS" ~ "negbinomial",
                            Measure %in% c("BAS","MON") ~ "zero_inflated_poisson"),
         Value = case_when(Measure %in% c("WBC","NEU","LYM","NLR") ~ log(Value),
                            Measure %in% c("BAS","EOS","MON") ~ Value))


m.NHANES <- matched.df %>% filter(Population == "NHANES") %>% 
  mutate(BAS = round(BAS), EOS = round(EOS)) %>% 
  select(Sex:BAS,NLR,pid,Repcat2,Repcat) %>% 
  pivot_longer(WBC:NLR, names_to = "Measure", values_to = "Value") %>% 
  mutate(Family = case_when(Measure %in% c("WBC","NEU","LYM","NLR","MON") ~ "gaussian",
                            Measure == "EOS" ~ "negbinomial",
                            Measure == "BAS" ~ "zero_inflated_poisson"),
         Value = case_when(Measure %in% c("WBC","NEU","LYM","NLR") ~log(Value),
                           Measure %in% c("MON","BAS","EOS") ~ Value))

M1 <- full_join(m.THLHP, m.NHANES)

split.M1 <- split(M1, list(M1$Measure, M1$Population))#M1$Repcat, 

#MODELS FOR EFFECTS OF AGE BY SEX, FOR EACH REPRODUCTIVE CATEGORY 
gen.models <- function(x){
  
  m1.df <- x %>% filter(Repcat == "Pre-pubertal")
  m2.df <- x %>% filter(Repcat == "Reproductive")
  m3.df <- x %>% filter(Repcat == "Post-Reproductive")

  #if(unique(x$Family) == "gaussian" & x$Measure != "MON"){
  #  x$Value <- log(x$Value)
  #} else {x$Value <- x$Value}

  if(x$Population == "NHANES"){
    fam <- unique(x$Family)
    
    #Pre-menarche models
    m1.prior <- get_prior(Value ~ Age*Sex+BMI, 
                          data = m1.df, family = fam)
    m1.prior$prior[1] <- "normal(0,10)"
    m1 <- brm(Value ~ Age*Sex+BMI,family = fam, 
              prior = m1.prior, data = m1.df,
              iter = 2500, chains=2, cores=2)
    #Reproductive models
    m2A.prior <- get_prior(Value ~ Age*Sex+BMI, data = m2.df, family = fam)
    m2A.prior$prior[1] <- "normal(0,10)"
    m2A <- brm(Value ~ Age*Sex+BMI,family = fam, 
              prior = m2A.prior, data = m2.df,
              iter = 2500, chains=2, cores=2)
    
    #Reproductive models, with repcat
    m2B.prior <- get_prior(Value ~ Age*Repcat2+BMI, data = m2.df, family = fam)
    m2B.prior$prior[1] <- "normal(0,10)"
    m2B <- brm(Value ~ Age*Repcat2+BMI,family = fam, 
               prior = m2B.prior, data = m2.df,
               iter = 2500, chains=2, cores=2)
    #Post-reproductive models
    m3.prior <- get_prior(Value ~ Age*Sex+BMI, data = m3.df, family = fam)
    m3.prior$prior[1] <- "normal(0,10)"
    m3 <- brm(Value ~ Age*Sex+BMI,family = fam, 
              prior = m3.prior, data = m3.df,
              iter = 2500, chains=2, cores=2)
    
  } else {
    fam <- unique(x$Family)
    #Pre-menarche models
    m1.prior <- get_prior(Value ~ Age*Sex+BMI+(1|pid), data = m1.df, family = fam)
    m1.prior$prior[1] <- "normal(0,10)"
    m1 <- brm(Value ~ Age*Sex+BMI+(1|pid),family = fam, 
              prior = m1.prior, data = m1.df,
              iter = 2500, chains=2, cores=2)
    #Reproductive models
    m2A.prior <- get_prior(Value ~ Age*Sex+BMI+(1|pid), data = m2.df, family = fam)
    m2A.prior$prior[1] <- "normal(0,10)"
    m2A <- brm(Value ~ Age*Sex+BMI+(1|pid),family = fam, 
               prior = m2A.prior, data = m2.df,
               iter = 2500, chains=2, cores=2)
    
    #Reproductive models, with repcat
    m2B.prior <- get_prior(Value ~ Age*Repcat2+BMI+(1|pid), data = m2.df, family = fam)
    m2B.prior$prior[1] <- "normal(0,10)"
    m2B <- brm(Value ~ Age*Repcat2+BMI+(1|pid),family = fam, 
               prior = m2B.prior, data = m2.df,
               iter = 2500, chains=2, cores=2)
    #Post-reproductive models
    m3.prior <- get_prior(Value ~ Age*Sex+BMI+(1|pid), data = m3.df, family = fam)
    m3.prior$prior[1] <- "normal(0,10)"
    m3 <- brm(Value ~ Age*Sex+BMI+(1|pid),family = fam, 
              prior = m3.prior, data = m3.df,
              iter = 2500, chains=2, cores=2)  
    }
  
  models <- list(m1,m2A,m2B,m3)
  names(models) <- paste0(unique(x$Population),".",
                          unique(x$Measure),
                          ".",c("M1","M2A","M2B","M3"))
  #models <- lapply(models, 
   #      function(x) mutate(x, Label = paste0(unique(x$Population),".",unique(x$Measure))))
  models
}


#test <- map(split.M1[c("NLR.THLHP","NLR.NHANES")],gen.models)
neu.models <- map(split.M1[c("NEU.THLHP","NEU.NHANES")],gen.models)
wbc.models <- map(split.M1[c("WBC.THLHP","WBC.NHANES")],gen.models)
lym.models <- map(split.M1[c("LYM.THLHP","LYM.NHANES")],gen.models)
nlr.models <- map(split.M1[c("NLR.THLHP","NLR.NHANES")],gen.models)
eos.models <- map(split.M1[c("EOS.THLHP","EOS.NHANES")],gen.models)
mon.models <- map(split.M1[c("MON.THLHP","MON.NHANES")],gen.models)
bas.models <- map(split.M1[c("BAS.THLHP","BAS.NHANES")],gen.models)

model.list <- c(neu.models, wbc.models, lym.models, 
                nlr.models, eos.models,mon.models,bas.models)

save.image("Chap3.RData")
