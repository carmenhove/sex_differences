#NHANES STUFF
#Create functional age groups, based on female reproductive phases
NHdata.2$RepPhase <-  NULL
#Under the age of 8 or explicitly report that period hasn't started yet = pre-menses (0-11 years)
NHdata.2$RepPhase[NHdata.2$Age < 8 | NHdata.2$MCQ149 == 2 | NHdata.2$RHQ020==0]<-1
#regularly cycling OR pregnant OR breastfeeding = post-menses/pre-menopause (12-68 years)
NHdata.2$RepPhase[NHdata.2$RHQ031 == 1 | NHdata.2$RHD143 == 1 | NHdata.2$RIDEXPRG==1 | NHdata.2$RHQ200 == 1] <- 2
#REMOVE individuals using hormonal supplements
#Using estrogen now? RHQ554 Using progestin now? RHQ562 Taking both? RHA574 Using estrogen patches? RHQ574
#Using both patches? RHQ600
NHdata.2$RepPhase[NHdata.2$RHQ554==1 | NHdata.2$RHQ562==1 | NHdata.2$RHA574 ==1 | 
                    NHdata.2$RHA574==1 | NHdata.2$RHA600==1]<-NA
#no longer regularily cycling, specifically due to menopause = menopause (40-80)
NHdata.2$RepPhase[NHdata.2$RHD043==7 | NHdata.2$RHD042 == 7]<-3
#males
NHdata.2$RepPhase[NHdata.2$Sex==0]<-4
table(NHdata.2$RepPhase)

# Clean up confusing coding in NumPreg and NumPartos vectors
## Code 77 (refused) and 99 (don't know) as NAs
NHdata.2$NumPartos[NHdata.2$NumPartos %in% c(77,99)]<-NA
NHdata.2$NumPreg[NHdata.2$NumPreg %in% c(77,99)]<-NA
#Make sure pre-menses individuals have zero pregnancies
NHdata.2$NumPreg[NHdata.2$RepPhase == 1 & NHdata.2$Sex==1]<-0
NHdata.2$NumPartos[NHdata.2$RepPhase == 1 & NHdata.2$Sex==1]<-0
#Make sure males have "Male" value for # of pregnancies + births
NHdata.2$NumPreg[NHdata.2$Sex==0]<-0
NHdata.2$NumPartos[NHdata.2$Sex==0]<-0

#Remove biological implausible levels of estrogen (outliers)
NHdata.2$Estrogen[NHdata.2$Estrogen >500]<-NA

NHdata.grouped<-NHdata.2[NHdata.2$Age >= 18 & NHdata.2$Age <=50,] %>%
  mutate(Group = NA)


#male
NHdata.grouped$Group[NHdata.grouped$Sex==0]<-"Male"
#regularily cycling and nulliparous
NHdata.grouped$Group[NHdata.grouped$RHQ031 == 1 & NHdata.grouped$RHQ131==2]<-"Nulliparous cycling"
#regularily cycling and not nulliparous
NHdata.grouped$Group[NHdata.grouped$RHQ031 == 1 & NHdata.grouped$RHQ131==1]<-"Parous cycling"
#pregnant
NHdata.grouped$Group[NHdata.grouped$RHD143 == 1 | NHdata.grouped$RIDEXPRG==1]<-"Pregnant"
#postpartum
NHdata.grouped$Group[NHdata.grouped$RHQ197 <= 6]<-"Postpartum"

NHdata.grouped$Group<-as.ordered(NHdata.grouped$Group)


ggplot(NHdata.grouped[!is.na(NHdata.grouped$Group),], aes(y = log(NEU), x = Group, fill = Group)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log(NEU), color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # geom_vline(xintercept= median(log(NHdata.grouped[NHdata.grouped$Group=="Male",]$WBC), na.rm=)) + 
  #expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme



#Need to remove all NAs in essential covariates before age-matching
NHdata.3<- NHdata.2[!is.na(NHdata.2$BMI) & !is.na(NHdata.2$NumPreg) & !is.na(NHdata.2$NumPartos),] 

#Age-match males and females for each female-based functional reproductive group
NH.phase1<-NHdata.3[NHdata.3$RepPhase %in% c(1,4),] %>%
  select(Sex, Age, pid, NEU, crp, IgE) %>%
  mutate(Group = "Pre-Menses")
NH.phase2<-NHdata[NHdata$RepPhase %in% c(2,4),] %>%
  select(Sex, Age, pid, NEU, crp, IgE)%>%
  mutate(Group = "Cycling/Preg/BF")
NH.phase3<-NHdata[NHdata$RepPhase %in% c(3,4),] %>%
  select(Sex, Age, pid, NEU, crp, IgE)%>%
  mutate(Group = "Menopause")

NHdata.4 <- full_join(NH.phase1, NH.phase2) %>%
  full_join(., NH.phase3)

#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
NH.WBC1 <- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Pre-Menses",c(1:4,7)]), 
                   discard = "control")
NH.WBC1m  <- full_join(match.data(NH.WBC1), NHdata.3)
#Post-menses age-matched dataframe for WBC data (matched n = 3628)
NH.WBC2 <- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Cycling/Preg/BF",c(1:4,7)]), 
                   discard = "control")
NH.WBC2m  <- full_join(match.data(NH.WBC2), NHdata.3)
#Menopausal age-matched dataframe for WBC data (matched n = 4488)
NH.WBC3 <- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Menopause",c(1:4,7)]), 
                   discard = "control")
NH.WBC3m <- full_join(match.data(NH.WBC3), NHdata.3)

NH.WBC<-full_join(NH.WBC1m, NH.WBC2m) %>%
  full_join(., NH.WBC3m)

NH.WBC$NumPartos[NH.WBC$Sex==0 & NH.WBC$Group=="Cycling/Preg/BF"]<-median(NH.WBC[NH.WBC$Sex==1 & NH.WBC$Group=="Cycling/Preg/BF",]$NumPartos, na.rm = T)
NH.WBC$NumPartos[NH.WBC$Sex==0 & NH.WBC$Group=="Menopause"]<-median(NH.WBC[NH.WBC$Sex==1 & NH.WBC$Group=="Menopause",]$NumPartos, na.rm = T)


#CRP
#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
NH.CRP1 <- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Pre-Menses",c(1:3,5,7)]),
                   discard = "control")
NH.CRP1m  <- full_join(match.data(NH.CRP1), NHdata.3)
#Post-menses age-matched dataframe for WBC data (matched n = 3628)
NH.CRP2<- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Cycling/Preg/BF",c(1:3,5,7)]),
                  discard = "control")
NH.CRP2m   <- full_join(match.data(NH.CRP2), NHdata.3)
#Menopausal age-matched dataframe for WBC data (matched n = 1336)
NH.CRP3 <- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Menopause",c(1:3,5,7)]),
                   discard = "control")
NH.CRP3m   <- full_join(match.data(NH.CRP3), NHdata.3)

NH.CRP<-full_join(NH.CRP1m, NH.CRP2m,) %>%
  full_join(., NH.CRP3m) 

NH.CRP$NumPartos[NH.CRP$Sex==0 & NH.CRP$Group=="Cycling/Preg/BF"]<-median(NH.CRP[NH.CRP$Sex==1 & NH.CRP$Group=="Cycling/Preg/BF",]$NumPartos, na.rm = T)
NH.CRP$NumPartos[NH.CRP$Sex==0 & NH.CRP$Group=="Menopause"]<-median(NH.CRP[NH.CRP$Sex==1 & NH.CRP$Group=="Menopause",]$NumPartos, na.rm = T)

#IgE
#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
NH.IGE1 <- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Pre-Menses",c(1:3,6:7)]),
                   discard = "control")
NH.IGE1m  <- full_join(match.data(NH.IGE1), NHdata.3)
#Post-menses age-matched dataframe for WBC data (matched n = 3628)
NH.IGE2<- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Cycling/Preg/BF",c(1:3,6:7)]),
                  discard = "control")
NH.IGE2m   <- full_join(match.data(NH.IGE2), NHdata.3)
#Menopausal age-matched dataframe for WBC data (matched n = 1336)
NH.IGE3 <- matchit(Sex ~ Age, data = na.omit(NHdata.4[NHdata.4$Group=="Menopause",c(1:3,6:7)]),
                   discard = "control")
NH.IGE3m   <- full_join(match.data(NH.IGE3), NHdata.3)

NH.IGE<-full_join(NH.IGE1m, NH.IGE2m,) %>%
  full_join(., NH.IGE3m) 

NH.IGE$NumPartos[NH.IGE$Sex==0 & NH.IGE$Group=="Cycling/Preg/BF"]<-median(NH.IGE[NH.IGE$Sex==1 & NH.IGE$Group=="Cycling/Preg/BF",]$NumPartos, na.rm = T)
NH.IGE$NumPartos[NH.IGE$Sex==0 & NH.IGE$Group=="Menopause"]<-median(NH.IGE[NH.IGE$Sex==1 & NH.IGE$Group=="Menopause",]$NumPartos, na.rm = T)


NH.WBC$Measure <- "WBC"
NH.CRP$Measure <- "CRP"
NH.IGE$Measure <- "IgE"

NH.dataset<-full_join(NH.WBC, NH.CRP) %>%
  full_join(., NH.IGE) %>%
  mutate(Group = ordered(Group, levels = c("Pre-Menses","Cycling/Preg/BF","Menopause")))




#DESCRIPTIVE STATS
#turn off scientific notation for tables
options(scipen = 999)

#create summarizing function
desc.formint<-function(x){
  paste0(format(signif(median(x, na.rm = TRUE)),digits=2)," (",format(signif(min(x, na.rm = TRUE)), digits=2),"-",format(signif(max(x, na.rm = TRUE)),digits=2),")")
}

NHANES_desc<-NH.dataset %>%
  select(Measure, Sex, BMI, Age, NumPreg, NumPartos, Group, pid, Estrogen, Testosterone) %>%
  mutate(Measure = factor(Measure),
         Sex = factor(Sex)) %>%
  group_by(Measure, Sex, Group) %>%
  summarise(BMI = desc.formint(BMI), Age = desc.formint(Age), 
            Parity_live = desc.formint(NumPartos), 
            Parity_gest = desc.formint(NumPreg),
            Estrogen = desc.formint(Estrogen),
            Testosterone = desc.formint(Testosterone),
            RM = as.character(length(pid)-length(unique(pid))),
            N = as.character(length((pid)))) %>%
  group_by(Measure) %>%
  arrange(Group, .by_group = FALSE) %>%
  select(-RM) 

write.csv(NHANES_desc, "NHANES_desc.csv")

## From demographic info - RIDEXPRG (2005-2007): Pregnancy status at the time of the health examination was ascertained for females 8–59 years of age. Due to disclosure risks pregnancy status is only released for women 20-44 years of age. The information used to code RIDEXPRG values included self-reported pregnancy status and urine pregnancy test results. Persons who reported they were pregnant at the time of exam were assumed to be pregnant (RIDEXPRG=1). Those who reported they were not pregnant or did not know their pregnancy status were further classified based on the results of the urine pregnancy test. If the respondent reported “no” or “don’t know” and the urine test result was positive, the respondent was coded as pregnant (RIDEXPRG=1). If the respondent reported “no” and the urine test was negative, the respondent was coded not pregnant (RIDEXPRG=2). If the respondent reported did not know her pregnancy status and the urine test was negative, the respondent was coded “could not be determined” (RIDEXPRG=3). Persons who were interviewed, but not examined also have an RIDEXPRG value = 3 (could not be determined).
##From reproductive health questionnaire - RHD143 = "Are you pregnant now?"


##---------------------------------------------------------------------------
#NOTES for THLHP dataset
##1. Very few females in the 10-15 age group for some reason


#Clean up number of live births variable: 101 is clearly a typo
TSdata.1$NumPartos[TSdata.1$NumPartos==101]<-10 
#Give males values for # pregnancies / births
TSdata.1$NumGestas[TSdata.1$Sex==0]<-0
TSdata.1$NumPartos[TSdata.1$Sex==0]<-0
#pre-menstruating women = zero pregnancies / births
TSdata.1$NumGestas[TSdata.1$Trimester=="Pre-Menst"]<-0
TSdata.1$NumPartos[TSdata.1$Trimester=="Pre-Menst"]<-0

#rename variables to match NHANES dataset, transform 1000 cells/uL to cells/uL
TSdata.2<-TSdata.1[!is.na(TSdata.1$BMI) & !is.na(TSdata.1$NumGestas) & 
                     !is.na(TSdata.1$NumPartos) & !is.na(TSdata.1$Age) & 
                     !is.na(TSdata.1$Sex),] 
#Set RepPhase vector to match NHANES
TSdata$RepPhase[TSdata$Trimester == "Pre-Menst"]<-1
TSdata$RepPhase[TSdata$Trimester %in% c("Cycling","Lactating","1","2","3") & 
                  TSdata$Trimester != "Pre-Menst" & TSdata$Trimester != "Menopause"]<-2
TSdata$RepPhase[TSdata$Trimester == "Menopause" | TSdata$Menopause == 1]<-3
TSdata$RepPhase[TSdata$Sex==0]<-4
table(TSdata$RepPhase)


#AGE MATCH BY SEX
#WBC (neutrophil)
#CRP
#IgG and IgE group together
#CD4, CD8, BCount, NKCount, 
#NaiveCD4 and Naive CD8 go together
#Senescent CD4 and CD8 go together
#VSG 
##e.g. unique(length(TSdata[!is.na(TSdata$NEU),]$pid) - length(TSdata[!is.na(TSdata$CD4Count),]$pid))

#Age-match males and females for each female-based functional reproductive group
TS.phase1<-TSdata[TSdata$RepPhase %in% c(1,4),] %>%
  select(Sex, Age, pid, NEU, crp, IgG, CD4Count, NaiveCD4Count, SenescentCD4Count) %>%
  mutate(Group = "Pre-Menses")
TS.phase2<-TSdata[TSdata$RepPhase %in% c(2,4),] %>%
  select(Sex, Age, pid, NEU, crp, IgG, CD4Count, NaiveCD4Count, SenescentCD4Count) %>%
  mutate(Group = "Cycling/Preg/BF")
TS.phase3<-TSdata[TSdata$RepPhase %in% c(3,4),] %>%
  select(Sex, Age, pid, NEU, crp, IgG, CD4Count, NaiveCD4Count, SenescentCD4Count) %>%
  mutate(Group = "Menopause")

TSdata.3 <- full_join(TS.phase1, TS.phase2) %>%
  full_join(., TS.phase3)

#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
WBC1 <- matchit(Sex ~ Age, data = na.omit(TSdata.3[TSdata.3$Group=="Pre-Menses",c(1:4,10)]),
                discard = "control")
WBC1.m  <- left_join(match.data(WBC1), TSdata)
#Post-menses (10-15) age-matched dataframe for WBC data (matched n = 3628)
WBC2 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Cycling/Preg/BF",c(1:4,10)]),
                discard = "control")
WBC2.m  <- left_join(match.data(WBC2), TSdata)
#Post-menses (15+) age-matched dataframe for WBC data (matched n = 3628)
WBC3 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Menopause",c(1:4,10)]),
                discard = "control")
WBC3.m  <- left_join(match.data(WBC3), TSdata)

TS.WBC<-full_join(WBC1.m, WBC2.m) %>%
  full_join(., WBC3.m)

#CRP
#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
CRP1 <- matchit(Sex ~ Age, data = na.omit(TSdata.3[TSdata.3$Group=="Pre-Menses",c(1:3,5,10)]),
                discard = "control")
CRP1.m  <- left_join(match.data(CRP1), TSdata)
#Post-menses (10-15) age-matched dataframe for WBC data (matched n = 3628)
CRP2 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Cycling/Preg/BF",c(1:3,5,10)]),
                discard = "control")
CRP2.m  <- left_join(match.data(CRP2), TSdata)
#Post-menses (15+) age-matched dataframe for WBC data (matched n = 3628)
CRP3 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Menopause",c(1:3,5,10)]),
                discard = "control")
CRP3.m  <- left_join(match.data(CRP3), TSdata)

TS.CRP<-full_join(CRP1.m, CRP2.m) %>%
  full_join(., CRP3.m) 

#IGE + IGG
#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
IG1 <- matchit(Sex ~ Age, data = na.omit(TSdata.3[TSdata.3$Group=="Pre-Menses",c(1:3,6,10)]),
               discard = "control")
IG1.m  <- left_join(match.data(IG1), TSdata)
#Post-menses (10-15) age-matched dataframe for WBC data (matched n = 3628)
IG2 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Cycling/Preg/BF",c(1:3,6,10)]),
               discard = "control")
IG2.m  <- left_join(match.data(IG2), TSdata)
#Post-menses (15+) age-matched dataframe for WBC data (matched n = 3628)
IG3 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Menopause",c(1:3,6,10)]),
               discard = "control")
IG3.m  <- left_join(match.data(IG3), TSdata)

TS.IG<-full_join(IG1.m, IG2.m) %>%
  full_join(., IG3.m) 

#CD4, CD8, B, NK
#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
LYM1 <- matchit(Sex ~ Age, data = na.omit(TSdata.3[TSdata.3$Group=="Pre-Menses",c(1:3,7,10)]),
                discard = "control")
LYM1.m  <- left_join(match.data(LYM1), TSdata)
#Post-menses (10-15) age-matched dataframe for WBC data (matched n = 3628)
LYM2 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Cycling/Preg/BF",c(1:3,7,10)]),
                discard = "control")
LYM2.m  <- left_join(match.data(LYM2), TSdata)
#Post-menses (15+) age-matched dataframe for WBC data (matched n = 3628)
LYM3 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Menopause",c(1:3,7,10)]),
                discard = "control")
LYM3.m  <- left_join(match.data(LYM3), TSdata)

TS.LYM<-full_join(LYM1.m, LYM2.m) %>%
  full_join(., LYM3.m) 


#NaiveCD4, CD8
#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
NAI1 <- matchit(Sex ~ Age, data = na.omit(TSdata.3[TSdata.3$Group=="Pre-Menses",c(1:3,8,10)]),
                discard = "control")
NAI1.m  <- left_join(match.data(NAI1), TSdata)
#Post-menses (10-15) age-matched dataframe for WBC data (matched n = 3628)
NAI2 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Cycling/Preg/BF",c(1:3,8,10)]),
                discard = "control")
NAI2.m  <-left_join(match.data(NAI2), TSdata)
#Post-menses (15+) age-matched dataframe for WBC data (matched n = 3628)
NAI3 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Menopause",c(1:3,8,10)]),
                discard = "control")
NAI3.m  <- left_join(match.data(NAI3), TSdata)

TS.NAI<-full_join(NAI1.m, NAI2.m) %>%
  full_join(., NAI3.m) 

#SENESCENT CELLS
#Pre-menses age-matched dataframe for WBC data (matched n = 2078)
SEN1 <- matchit(Sex ~ Age, data = na.omit(TSdata.3[TSdata.3$Group=="Pre-Menses",c(1:3,9:10)]),
                discard = "control")
SEN1.m  <- left_join(match.data(SEN1), TSdata)
#Post-menses (10-15) age-matched dataframe for WBC data (matched n = 3628)
SEN2 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Cycling/Preg/BF",c(1:3,9:10)]),
                discard = "control")
SEN2.m  <- left_join(match.data(SEN2), TSdata)
#Post-menses (15+) age-matched dataframe for WBC data (matched n = 3628)
SEN3 <- matchit(Sex ~ Age, data =na.omit(TSdata.3[TSdata.3$Group=="Menopause",c(1:3,9:10)]),
                discard = "control")
SEN3.m  <- left_join(match.data(SEN3), TSdata)

TS.SEN<-full_join(SEN1.m, SEN2.m) %>%
  full_join(., SEN3.m) 

#bind it all together
TS.dataset<-full_join(TS.WBC, TS.CRP) %>%
  full_join(., TS.IG) %>%
  full_join(., TS.LYM) %>%
  full_join(., TS.NAI) %>%
  full_join(., TS.SEN) %>%
  mutate(Group = ordered(Group, levels = c("Pre-Menses","Cycling/Preg/BF","Menopause")),
         Repeats = ave(rep(1,nrow(TS.dataset)),TS.dataset$pid,FUN=sum), 
         REF = ifelse(Repeats>1,1,0))


#DESCRIPTIVE STATS
THLHP_desc <- TS.dataset %>%
  select(Sex, BMI, Age, NumPartos, NumGestas, Group, pid, 
         NEU, crp, VSG, CD4Count, IgG, NaiveCD4Count, SenescentCD4Count) %>%
  pivot_longer(
    cols = NEU:SenescentCD4Count,
    names_to = "Measure"
  ) %>%
  select(-value) %>%
  mutate(Sex = factor(Sex),
         Group = ordered(Group, levels = c("Pre-Menses","Cycling/Preg/BF","Menopause")), 
         Measure = factor(Measure)) %>%
  group_by(Measure, Sex, Group) %>%
  summarise(BMI = desc.formint(BMI), Age = desc.formint(Age), 
            Preg = desc.formint(NumPartos), 
            LiveBirths = desc.formint(NumGestas), 
            RM = as.character(length(pid)-length(unique(pid))),
            N = as.character(length((pid)))) %>%
  group_by(Measure) %>%
  arrange(Group, .by_group = FALSE) 

write.csv(THLHP_desc, "THLHP_desc.csv")


##-------------------------------------------------------------------
# FIGURES

#TSIMANE graphs
TS.dataset.graph <- TS.dataset %>% 
  select(Sex, Age, Group, WBC,NEU,LYM,EOS,MON,BAS,crp,VSG,CD4Count,CD8Count,NKCount,BCount,IgE,IgG) %>%
  pivot_longer(
    cols = c(WBC:IgG),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  mutate(Measure =  ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON","BAS","crp","VSG","CD4Count","CD8Count","NKCount","BCount","IgE","IgG"))) %>%
  arrange(Measure) 

wbc<-c("WBC","NEU","LYM","EOS","MON","BAS")
inf<-c("crp","VSG")
lyms<-c("CD4Count","CD8Count","BCount","NKCount")
igs<-c("IgE","IgG")

Fig1<-
  ggplot(na.omit(TS.dataset.graph[TS.dataset.graph$Measure == wbc,]), 
         aes(x = Age, y = log(Value+1), color = Sex)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(Measure ~ Group, scales = "free") 

Fig2<-
  ggplot(TS.dataset.graph[TS.dataset.graph$Measure %in% lyms,], 
         aes(x = Age, y = log(Value), color = Sex)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(Measure ~ Group, scales = "free") 

Fig3<-
  ggplot(TS.dataset.graph[TS.dataset.graph$Measure %in% igs,], 
         aes(x = Age, y = log(Value), color = Sex)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(Measure ~ Group, scales = "free") 

Fig4<-
  ggplot(TS.dataset.graph[TS.dataset.graph$Measure %in% inf,], 
         aes(x = Age, y = log(Value), color = Sex)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(Measure ~ Group, scales = "free") 

#NHANES graphs
NH.dataset.graph <- NH.dataset %>% 
  select(Sex, Age, Group, NEU,LYM, WBC,EOS,MON,BAS,IgE,crp,Estrogen,
         Testosterone, NumPreg, NumPartos) %>%
  pivot_longer(
    cols = c(NEU:crp),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  mutate(Measure =  ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON","BAS","crp","IgE"))) %>%
  arrange(Measure) %>%
  filter(!is.na(Group))

Fig5<-
  ggplot(NH.dataset.graph[NH.dataset.graph$Measure %in% wbc,], 
         aes(x = Age, y = log(Value+1), color = Sex)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(Measure ~ Group, scales = "free") 

Fig6<-
  ggplot(NH.dataset.graph[NH.dataset.graph$Measure == "IgE",], 
         aes(x = Age, y = log(Value+1), color = Sex)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(~ Group, scales = "free") 

Fig7<-
  ggplot(NH.dataset.graph[NH.dataset.graph$Measure == "crp",], 
         aes(x = Age, y = log(Value), color = Sex)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(~ Group, scales = "free") 

#relationship between estrogen and WBCs in females
ggplot(NH.dataset.graph[NH.dataset.graph$Measure %in% wbc &
                          NH.dataset.graph$Sex==1,], 
       aes(x = log(Estrogen), y = log(Value+1), color = Measure)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(Measure ~ Group, scales = "free") 

#relationship between parity and measures in females
ggplot(NH.dataset.graph[NH.dataset.graph$Sex==1 & NH.dataset.graph$Group!= "Pre-Menses",], 
       aes(x = NumPartos, y = log(Value+1), color = Measure)) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(Measure ~ Group, scales = "free") 

#relationship between testosterone and WBCs in males
ggplot(NH.dataset.graph[NH.dataset.graph$Measure %in% wbc &
                          NH.dataset.graph$Sex==0,], 
       aes(x = log(Testosterone), y = log(Value+1))) + 
  geom_point(size=2, position="jitter") + 
  facet_grid(Measure ~ Group, scales = "free") 

#-------------------------------------------------------------
#MODELS

#THLHP Neutrophil count / functional age group
hist(log(TS.dataset[TS.dataset$Group=="Pre-Menses",]$NEU))
#calculate PRE-MENSES sexual dimorphism
m1<-gam(NEU ~ s(Age, by = Sex, bs = "ad") + s(BMI),
        data = TS.dataset[TS.dataset$Group=="Pre-Menses",], method = "REML")
m1v <- getViz(m1)
print(plot(m1v, allTerms = TRUE), pages = 1)
plotDiff(s1 = sm(m1v, 1), s2 = sm(m1v, 2)) + l_ciLine()+
  l_fitLine()+ geom_hline(yintercept = 0, linetype = 2)

#Same for Reproductive years
hist(log(TS.dataset[TS.dataset$Group=="Cycling/Preg/BF",]$NEU))
#calculate PRE-MENSES sexual dimorphism
m2<-gam(NEU ~ s(Age, by = Sex, bs = "ad") + s(BMI) +s(NumPartos, by = Sex),
        data = TS.dataset[TS.dataset$Group=="Cycling/Preg/BF",], 
        method = "REML")
m2v <- getViz(m2)
print(plot(m2v, allTerms = TRUE), pages = 1)
plotDiff(s1 = sm(m2v, 1), s2 = sm(m2v, 2)) + l_ciLine()+
  l_fitLine()+ geom_hline(yintercept = 0, linetype = 2)

#SAVE ALL OBJECTS
#ls()
save.image(file='SexDiffEnvironment.RData')
quit(save='no')
