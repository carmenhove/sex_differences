#----------------------------------------------------------------------------
# COMPILE FULL DATASET

# Create "population" variable before merging
TSdata$Population<-c(rep("THLHP",length(TSdata$pid)))
NHdata$Population<-c(rep("NHANES",length(NHdata$pid)))

TSdata$Sex<-as.factor(TSdata$Sex)

# Join NHANES and THLHP datasets 
Dataset.1<-full_join(NHdata, TSdata)

#Remove all NA values for key covariates
subset<-!is.na(Dataset.1$BMI) &!is.na(Dataset.1$Age) & !is.na(Dataset.1$Sex) & !is.na(Dataset.1$RepStatus)

Dataset.2<-Dataset.1[subset,]

Dataset <- Dataset.2 %>%
  # Make zero inflated/skewed values integers
  mutate(MON = as.integer(MON), BAS = as.integer(BAS)) %>%
  # Add percentages
  mutate(lym_pct = LYM/WBC * 100, neu_pct = NEU/WBC * 100, eos_pct = EOS/WBC * 100,
         mon_pct = as.integer(MON/WBC * 100), bas_pct = as.integer(BAS/WBC * 100)) %>% 
  # Add logged values  
  mutate(lnWBC = log(WBC), lnLYM = log(LYM), lnNEU = log(NEU), lnCRP = log(crp),
         lnEOS = log(EOS+1),lnMON = log(MON+1), lnBAS = log(BAS+1), 
         ln.lym_pct = log(lym_pct), ln.neu_pct = log(neu_pct), 
         ln.eos_pct = log(eos_pct+1), ln.mon_pct = log(mon_pct+1)) %>%
  # Add repeat values vector
  mutate(Repeats = ave(rep(1,nrow(Dataset.2)),Dataset.2$pid,FUN=sum), 
         REF = ifelse(Repeats>1,1,0)) #%>%
#select(c(Year, pid, WBC, lnWBC, NEU, lnNEU, LYM, lnLYM, 
#  lnBAS, BAS, lnEOS, EOS, IgE, lnCRP, crp, BMI, Age, 
#  NumPreg, NumPartos, Estrogen, Testosterone, HIV, CD4Count, 
#  CD8Count, CD4CD8, Sex, RepStatus, Population, IgG, VSG,
#  NaiveCD4Count, NaiveCD8Count, SenescentCD4Count, SenescentCD8Count,
# BCount, NKCount, Asthma.current))

#CD4:CD8
table(Dataset$RepStatus)
table(Dataset$Sex)

Dataset$Group <- NA
Dataset$Group[Dataset$Sex == 0 & Dataset$Population == "THLHP"] <- 1 #THLHP Male
Dataset$Group[Dataset$Sex == 1 & Dataset$Population == "THLHP"] <- 2 #THLHP Female
Dataset$Group[Dataset$Sex == 0 & Dataset$Population == "NHANES"] <- 3 #NHANES Male
Dataset$Group[Dataset$Sex == 1 & Dataset$Population == "NHANES"] <- 4 #NHANES Female
Dataset$Group<-as.factor(Dataset$Group)

NH.dataset<-Dataset[Dataset$Population=="NHANES",]
TS.dataset<-Dataset[Dataset$Population=="THLHP",]

#Age match by sex using propensity score
#NH.match <- NH.dataset %>%
#  select(c(Age, Sex, lnWBC, pid)) %>%
#  na.omit()
#NH.match2 <- matchit(Sex ~ Age, data = NH.match)
#summary(NH.match2)
#NH.matched <- match.data(NH.match2)

#NH.matched <- match.data(NH.match2)
#NH.dataset.m<-left_join(NH.matched, NH.dataset, by="pid")



# REMEMBER, 0 = male and 1 = female 
wbc.all<-gam(lnWBC ~ Group + s(Age, by = Group) + s(BMI),
             data=Dataset, method = "REML")
#m1_p<-predict_gam(m1)
wbc.all <- getViz(wbc.all)
print(plot(wbc.all, allTerms = TRUE), pages = 1)

# Change to 0 = male, 1 = female 
wbc.TS<-gam(CD4Count ~ s(Age, by = Sex) + s(BMI, by = Sex),
            data=TS.dataset, method = "REML")
wbc.TS <- getViz(wbc.TS)
print(plot(wbc.TS, allTerms = TRUE), pages = 1)
plotDiff(s1 = sm(wbc.TS, 1), s2 = sm(wbc.TS, 2)) + l_ciLine()+
  l_fitLine()+ geom_hline(yintercept = 0, linetype = 2)

# Change to 0 = male, 1 = female 
wbc.NH-gam(lnNEU ~ Sex + s(Age,by = Sex) + s(BMI), data=NH.dataset)
wbc.NH<- getViz(wbc.NH)
plotDiff(s1 = sm(wbc.NH, 1), s2 = sm(wbc.NH, 2)) + l_ciLine()+
  l_fitLine()+ geom_hline(yintercept = 0, linetype = 2)

#plot(sm(m1.plot, 1)) + l_fitLine(colour = "red") + 
#l_rug(mapping = aes(x=x, y=y),alpha = 0.8, size = 1) +
#l_ciLine(mul = 5, colour = "blue", linetype = 2, size = 1) + 
# l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()


plot1 <- plotGAM(gamFit = m1, smooth.cov = "Age", groupCovs = "Group",
                 rawOrFitted = "raw", plotCI=TRUE, orderedAsFactor = FALSE)

m1.plot <- ggplot(NH.dataset, aes(Age, log(NEU),  color = Sex)) +  geom_point(size=2, position="jitter", alpha = 0.1) + stat_smooth(method = "gam", formula = y ~ s(x), size = 1)


b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
b <- getViz(b)

check(m1.plot,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))  

#oh wow, BMI changes a ton over the lifecourse
plot(TS.dataset$Age, log(TS.dataset$BMI))
plot(NH.dataset$Age, log(NH.dataset$BMI))
#MCQ220 = diagnosis of cancer
#MCQ230a = type of cancer (1)
ggplot(NHdata[NHdata$MCQ230A %in% c(10,14,16,25,32,38,33),], aes(Age, log(LYM),  color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(MCQ230A ~., scales = "fixed") 

ggplot(NHdata[NHdata$Psoriasis %in% c(1:2),], aes(Age, log(NEU),  color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(Psoriasis ~., scales = "fixed") 

ggplot(NHdata[NHdata$Arthritis.current %in% c(1:2),], aes(Age, log(NEU),  color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(Arthritis.current ~., scales = "fixed") 

ggplot(NHdata, aes(Age, log(NEU),  color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(Arthritis.current ~., scales = "fixed") 

ggplot(NHdata[NHdata$Asthma.current %in% c(1:2),], aes(Age, log(IgE),  color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(Asthma.current ~., scales = "fixed") 

ggplot(NHdata[NHdata$Cancer %in% c(1:2),], aes(Age, log(Testosterone),  color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(Cancer ~., scales = "fixed") 

ggplot(NHdata[NHdata$Cancer %in% c(1:2),], aes(Age, log(NEU),  color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(Cancer ~., scales = "fixed") 

ggplot(NHdata[NHdata$HIV %in% c(1:2),], aes(Age, log(LYM),  color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(HIV ~., scales = "fixed") 

#LYM * Estrogen:Testosterone by HERPES SIMPLEX 1 status
NHdata$LBXHE2 <- as.factor(NHdata$LBXHE2)
ggplot(NHdata[NHdata$LBXHE1 %in% c(1:2),], aes(log(LYM), log(TestEst), color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(LBXHE1 ~., scales = "fixed") 

ggplot(hi.est[hi.est$LBXHE1 %in% c(1:2),], aes(log(LYM), log(Estrogen))) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(LBXHE1 ~., scales = "fixed") 


#CYTO VIRUS IgG antibody
ggplot(NHdata[!is.na(NHdata$Sex),], aes(Sex, log(LBXIGG))) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) 

ggplot(NHdata, aes(Age, log(Estrogen)), color = Sex) +  geom_point(size=2, position="jitter") 

#high and low estrogen groups
ggplot(hi.est, aes(Age, WBC)) +  geom_point(size=2, position="jitter") 
ggplot(lo.est, aes(Age, WBC)) +  geom_point(size=2, position="jitter") 
#red blob is simply a difference in relative sample size for young adult, no data are missing
check<-NH.dataset.m[NH.dataset.m$Age.x > 10 & NH.dataset.m$Age.x < 20,]
check.1<-NH.dataset[NH.dataset$Age > 10 & NH.dataset$Age < 20,]
ggplot(check.1, aes(Age, lnWBC,  color = Sex)) +  geom_point(size=2, position="jitter") + facet_grid(Sex ~., scales = "fixed")





ggplot(NH.WBC[NH.WBC$RepPhase==2,], aes(NumPartos, log(WBC))) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(~Sex.x, scales = "free_x") 

ggplot(NH.WBC[NH.WBC$RepPhase==2,], aes(Age.x, log(WBC), color = Sex.x)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(~Parity, scales = "free_x") 


ggplot(NH.CRP, aes(Age.x, log(crp), color = Sex.x)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(~Group, scales = "free_x") 


NH.IGE$Group<- ordered(NH.IGE$Group, levels = c("Pre-Menses", "Cycling/Preg/BF", "Menopause"))
ggplot(NH.IGE, aes(Age.x, log(IgE), color = Sex.x)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(~Group, scales = "free_x", labeller=labeller(Measure = labels, margins = TRUE))

#High- and low-parity females vs males
ggplot(NH.WBC[NH.WBC$Group %in% c("Cycling/Preg/BF","Menopause"),], aes(Age.x, log(NEU), color = Sex.2)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(~Group, scales = "free_x") 
#scale_fill_manual(values=c("#D67236","#1d4e89",adjustcolor( "#D67236", alpha.f = 0.2)))+
# scale_color_manual(values=c("#D67236","#1d4e89"))+










ggplot(TS.dataset, aes(Age, log(NaiveCD4Count), color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(~Group, scales = "free_x") 

ggplot(NH.dataset, aes(Age, log(CD8Count), color = Sex)) +  geom_point(size=2, position="jitter") + scale_shape_manual(values = c(0, 19)) + facet_grid(~Group, scales = "free_x") 