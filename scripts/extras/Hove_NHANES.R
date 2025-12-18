
#Load previously 
load("Chapter2.RData")

#PACKAGES
pkgs = c("nhanesA","tidyverse","ggplot2","sjlabelled","Hmisc","brms","MatchIt","optmatch")
install.packages(pkgs)
inst = lapply(pkgs, library, character.only = TRUE) 


#COMPILE NHANES DATASET
#DEMOGRAPHICS
Demographics<-full_join(remove_all_labels(nhanes('DEMO')),
                        remove_all_labels(nhanes('DEMO_B'))) %>%
  full_join(., remove_all_labels(nhanes('DEMO_C'))) %>%
  full_join(., remove_all_labels(nhanes('DEMO_D'))) %>%
  full_join(., remove_all_labels(nhanes('DEMO_E'))) %>% 
  full_join(., remove_all_labels(nhanes('DEMO_G'))) %>%
  full_join(., remove_all_labels(nhanes('DEMO_H'))) %>%
  full_join(., remove_all_labels(nhanes('DEMO_I'))) %>%
  full_join(., remove_all_labels(nhanes('DEMO_J'))) 

label(Demographics$SEQN) <- "Respondent sequence number" #add appropriate label to SEQN variable

#REPRODUCTIVE HEALTH
#Currently pregnant = RHD143
#Regular periods in last 12 months? = RHQ031

#EVER been pregnant? = RHQ131
#How many times have you been pregnant?  = RHQ160
#How many times live birth? = RHQ171

RHQ_B = nhanes('RHQ_B') %>%
  rename(RHD042 = RHQ040, #reason no regular periods
         RHQ031 = RHQ030, #having regular periods
         RHD143 = RHQ141) %>% 
  mutate(file = "RHQ_B") %>% #do you think that you are pregnant now?
  select(-RHD470)

RHQ = nhanes('RHQ') %>%
  rename(RHD042 = RHQ040, #reason no regular periods
         RHQ031 = RHQ030, #having regular periods
         RHD143 = RHQ140) %>% 
  mutate(file = "RHQ") %>% #do you think that you are pregnant now?
  select(-RHD470) 

RepHealth<-full_join(remove_all_labels(RHQ), remove_all_labels(RHQ_B)) %>%
  full_join(., remove_all_labels(nhanes('RHQ_C'))) %>% 
  full_join(., remove_all_labels(nhanes('RHQ_D'))) %>% 
  full_join(., remove_all_labels(nhanes('RHQ_E'))) %>% 
  full_join(., remove_all_labels(nhanes('RHQ_F'))) %>% #RHD042 = reason no regular periods
  full_join(., remove_all_labels(nhanes('RHQ_G'))) %>%
  full_join(., remove_all_labels(nhanes('RHQ_H')) %>% rename(RHD042 = RHD043)) %>%
  full_join(., remove_all_labels(nhanes('RHQ_I')) %>% rename(RHD042 = RHD043)) %>% #RHD
  full_join(., remove_all_labels(nhanes('RHQ_J')) %>% rename(RHD042 = RHD043))

#set "menopause" answer to 7 across all years
RepHealth$RHD042[RepHealth$file %in% c("RHQ","RHQ_B") & RepHealth$RHD042==5]<-7

label(RepHealth$SEQN) <- "Respondent sequence number" #add appropriate label to SEQN variable

#COMPLETE BLOOD COUNT
#CBC_D = "Platelet count SI (1000 cells/uL)"
#CBC_E/F/G/H/I = ""Platelet count (1000 cells/uL)"

#Relabel Platelet Count variable in CBC_D
CBC_D = nhanes('CBC_D')
label(CBC_D$LBXPLTSI) <- "Platelet count (1000 cells/uL)"

#Relabel 
CBC_J = nhanes('CBC_J')
label(CBC_J$LBXMC)<-"MCHC (g/dL)"

CBCdata<-full_join(CBC_D, nhanes('CBC_E')) %>% #"Mean platelet volume (fL)"
  full_join(., nhanes('CBC_F')) %>%
  full_join(., nhanes('CBC_G')) %>%
  full_join(., nhanes('CBC_H')) %>%
  full_join(., nhanes('CBC_I')) %>%
  full_join(., CBC_J) %>%
  filter(LBXWBCSI<30)

#C-REACTIVE PROTEIN
CRPdata<-full_join(nhanes('L11_B'), nhanes('L11_C')) %>%
  full_join(., nhanes('CRP_D')) %>%
  full_join(., nhanes('CRP_E')) %>%
  full_join(., nhanes('CRP_F')) 

#add BMI
BMIdata <- full_join(nhanes('BMX_C'), nhanes('BMX_D')) %>%
  full_join(., nhanes('BMX_E')) %>%
  full_join(., nhanes('BMX_F')) %>%
  full_join(., nhanes('BMX_G') %>% select(-BMDBMIC)) %>%
  full_join(., nhanes('BMX_H') %>% select(-BMDBMIC)) %>%
  full_join(., nhanes('BMX_I') %>% select(-BMDBMIC))

#SEX HORMONES
#TS_G is no longer available, so I removed it. 
#I added surplus data files.
## In SSCHL_A/B/C, testosterone and estradiol have (1) different variables names and (2) are provided in different units than in subsequent years. I've changed these so that they merge properly. 

## LBXTST = testosterone (ng/dL)
## LBXEST = estradiol (pg/mL)
H1<-full_join(nhanes('TST_H'), nhanes('TST_I')) %>%
  rename(Testosterone = LBXTST, #rename to testosterone
         Estradiol = LBXEST) %>% #rename to estradiol
  select(SEQN, Testosterone, Estradiol) #select these variables

## SSTESTO = testosterone (ng/dL)
## SSSE2 = estradiol (pg/mL)
H2 <- nhanes('SSCHL_C') %>%
  mutate(Testosterone = SSTESTO*100, #rename to testosterone
         Estradiol = SSSE2) %>% #rename to estradiol
  select(SEQN, Testosterone, Estradiol)

## SSTESTO = testosterone
## SSSE2 = estradiol (pg/mL)
H3<-full_join(nhanes('SSCHL_A'), nhanes('SSCHL_B')) %>% 
  mutate(Testosterone = SSTESTO*100, #Testostosterone (ng/mL -> ng/dL)
         Estradiol = SSSE2) %>% #rename to estradiol
  select(SEQN, Testosterone, Estradiol) #select these variables

#Change labels so that they are all matching
label(H2$Testosterone) <- "Testosterone, total (ng/dL)"
label(H3$Testosterone) <- "Testosterone, total (ng/dL)"
label(H1$Estradiol) <- "estradiol (pg/mL)"

#SEX HORMONES - FINAL DATASET
SexHormones <- full_join(H1, H2) %>%
  full_join(., H3)

#MEDICAL CONDITIONS
MedicalCond<-full_join(remove_all_labels(nhanes('MCQ_B')) %>% rename(MCQ195 = MCQ190),
                       remove_all_labels(nhanes('MCQ_C')) %>% rename(MCQ195 = MCQ190)) %>% 
  full_join(., remove_all_labels(nhanes('MCQ_D')) %>% rename(MCQ195 = MCQ190)) %>%
  full_join(., remove_all_labels(nhanes('MCQ_E')) %>% rename(MCQ195 = MCQ190)) %>% 
  full_join(., remove_all_labels(nhanes('MCQ_F')) %>% rename(MCQ195 = MCQ191)) %>%
  full_join(., remove_all_labels(nhanes('MCQ_G'))) %>%
  full_join(., remove_all_labels(nhanes('MCQ_H'))) %>%
  full_join(., remove_all_labels(nhanes('MCQ_I')))

label(MedicalCond$SEQN) <- "Respondent sequence number"

##------------------------------
# MERGED NHANES INTIAL DATAFRAME
NHdata1<-full_join(Demographics, RepHealth) %>%
  full_join(., CBCdata) %>%
  full_join(., CRPdata) %>%
  full_join(., BMIdata) %>%
  full_join(., SexHormones) %>%
  full_join(., MedicalCond)

##------------------------------
NHdata <- NHdata1 %>%
  mutate(Sex = fct_recode(factor(RIAGENDR), "Female" = "2", "Male" = "1"),
         Preg.rhq = fct_recode(factor(RHD143), "Yes"="1", "No"="2"),
         Preg.demo = fct_recode(factor(RIDEXPRG), "Yes"="1", "No"="2"), 
         Reg.periods = fct_recode(factor(RHQ031),"Yes"="1", "No"="2"),
         EverPreg = fct_recode(factor(RHQ131),"Yes"="1", "No"="2"),
         BF = fct_recode(as.factor(RHQ200),"Yes"="1", "No"="2"),
         Menarche = fct_recode(as.factor(MCQ149),"Yes"="1", "No"="2"),
         BMI = BMXBMI, Age = RIDAGEYR, 
         WBC = LBXWBCSI*1000, 
         NEU = LBDNENO*1000, LYM = LBDLYMNO*1000, 
         MON = LBDMONO*1000, BAS = LBDBANO*1000, 
         EOS = LBDEONO*1000, crp = LBXCRP*10, 
         #IgE = as.numeric(LBXIGE), 
         pid = as.character(SEQN), 
         NumPreg = abs(RHQ160),  NumPartos = abs(RHQ171), 
         MonthPreg = abs(RHD152),  MonthPP = abs(RHQ197),
         Estradiol = abs(Estradiol), Testosterone = abs(Testosterone),
         lnEstradiol = log(Estradiol),  lnTestosterone = log(Testosterone),
         EstTest = log(Estradiol/Testosterone)) %>%
  filter(!is.na(Age), !is.na(BMI))

NHdata$Sex <- factor(NHdata$Sex, levels = c("Female", "Male"))

#RHQ051 = when did you have last period? 1 = now, 2 = less than 2 months ago
#RHQ554==1, -RHQ562==1, -RHQ574==1, -RHQ574==1, -RHQ600==1)
#Remove biological implausible levels of estrogen (outliers)
#NHdata.2$Estrogen[NHdata.2$Estrogen >500]<-NA

##----------------------------------------------------------
# Clean up confusing coding in NumPreg, NumPartos, MonthPreg, MonthPP vectors
## Code 77 (refused), 99 (don't know) as NAs
NHdata$NumPartos[NHdata$NumPartos %in% c(77,99)]<-NA 
NHdata$NumPreg[NHdata$NumPreg %in% c(77,99)]<-NA
NHdata$MonthPreg[NHdata$MonthPreg %in% c(77,99,10)]<-NA
NHdata$MonthPP[NHdata$MonthPP %in% c(77,99)]<-NA

#create full pregnancy variable
NHdata$Preg[NHdata$Preg.demo=="Yes" | NHdata$Preg.rhq=="Yes"]<-"Yes" 
NHdata$Preg[NHdata$Preg.demo=="No" | NHdata$Preg.rhq=="No"]<-"No" 
NHdata$Preg <- as.factor(NHdata$Preg)

#__________________________________________________________________
#Create functional age groups, based on female reproductive phases
NHdata$RepPhase <-  NULL
#Under the age of 8 or explicitly report that period hasn't started yet = pre-menses (0-11 years) #RHQ010 = age when first menstrual period happened (0 = hasn't happened yet)
#no longer regularily cycling due to menopause = menopause (40-80)
NHdata$RepPhase[NHdata$Menarche == "No" | NHdata$RHQ010 ==0]<-"Pre-menses"
NHdata$RepPhase[NHdata$Age < 8 & NHdata$Sex == "Female"] <- "Pre-menses"
NHdata$RepPhase[NHdata$Reg.periods == "Yes" | 
                  NHdata$RHQ051 %in% c(1,2) |
                  NHdata$Preg=="Yes" | 
                  NHdata$BF == "Yes"  | 
                  NHdata$MonthPP<=24] <- "Cyc-Preg-PP"
NHdata$RepPhase[NHdata$RHD042 == 7]<-"Menopause"
NHdata$RepPhase[NHdata$Sex=="Male"]<-"Male"
table(NHdata$RepPhase)

#CREATE CYC-PREG-PP VARIABLE
NHdata$Group <- NULL
NHdata$Group[NHdata$Sex=="Male" & NHdata$Age >=18 & NHdata$Age <=50]<-"Male"
NHdata$Group[NHdata$EverPreg=="No" & NHdata$Reg.periods=="Yes"]<-"Nulliparous cycling"
NHdata$Group[NHdata$EverPreg=="Yes" & NHdata$Reg.periods=="Yes"]<-"Parous cycling"
NHdata$Group[NHdata$Preg=="Yes" & NHdata$NumPreg==1]<-"Primiparous Pregnant"
NHdata$Group[NHdata$Preg=="Yes" & NHdata$NumPreg>1]<-"Multiparous Pregnant"
NHdata$Group[NHdata$MonthPP <= 24]<-"Postpartum"


#FIX NUMBER OF LIVE BIRTHS VARIABLE BY ADDING MISSED NULLIPAROUS INDIVIDUAL
#Give people who answered "NO" to "ever been pregnant?" a live birth value of zero
#people who have (1) never been pregnant or (2) never had a live birth == not currently breastfeeding
#people who have never been pregnant == not currently pregnant
NHdata$NumPartos[NHdata$EverPreg=="No" | NHdata$RepPhase == "Pre-menses" | NHdata$Sex=="Male"]<-0
NHdata$NumPreg[NHdata$Sex=="Male" | NHdata$RepPhase == "Pre-menses"]<-0
NHdata$BF[NHdata$EverPreg=="No" | NHdata$NumPartos == 0 | NHdata$Sex=="Male" | NHdata$RepPhase == "Pre-menses"]<-"No"
NHdata$Preg[NHdata$EverPreg=="No" | NHdata$Sex=="Male" | NHdata$RepPhase == "Pre-menses"]<-"No"


#subset
NHdataF <- NHdata %>%
  filter(!is.na(NumPartos),
         !is.na(NumPreg),
         !is.na(Sex))

#break into measure-specific 
NH.crp <- NHdataF %>%
  filter(!is.na(crp) & !is.na(RepPhase)) %>%
  mutate(Measure = "C-reactive Protein")

NH.wbc <- NHdataF %>%
  filter(!is.na(WBC),!is.na(LYM),
         !is.na(NEU),!is.na(EOS),
         !is.na(BAS),!is.na(MON),
         !is.na(RepPhase)) %>%
  mutate(Measure = "Leukocyte differential")

#put it back together
NHANES <- full_join(NH.crp, NH.wbc) %>%
  select(Sex:EstTest, Group, RepPhase, Measure) %>%
  mutate(Population = "NHANES")

save.image(file = "Chapter2.RData")

