
#Sex differences
wbc.ts <-brm(log(WBC) ~ s(Age, by = Sex, k = 4)+BMI, 
         knots = list(RepMonth.adj = c(0,15,45,90)),
         data=THLHP, family = gaussian(),
         iter = 2000, warmup = 500,
         chains=2, cores=2)

wbc.nh <- brm(log(WBC) ~ s(Age, by = Sex, k =4)+BMI, 
              knots = list(RepMonth.adj = c(0,15,45,90)),
              data=NHANES, family = gaussian(),
             iter = 1000, warmup = 500,
             chains=2, cores=2)

plot(conditional_effects(wbc.nh),points = FALSE, rug = FALSE)
plot(conditional_effects(wbc.ts),points = FALSE, rug = FALSE)

quantile(THLHP[THLHP$RepPhase=="Pre-menses",]$Age)
quantile(THLHP[THLHP$RepPhase=="Cyc-Preg-PP",]$Age)
range(NHANES[NHANES$RepPhase=="Cyc-Preg-PP",]$Age)

range(THLHP[THLHP$RepPhase=="Menopause",]$Age)
