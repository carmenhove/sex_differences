#THLHP

#HORMONE THLHP DATASET
TSdata.cm <- read.csv("cm_data.csv", stringsAsFactors = FALSE)

#change values
TSdata.cm$t_pgml[TSdata.cm$t_pgml == "<625"]<-625
TSdata.cm$t_pgml[TSdata.cm$t_pgml == "<625.36"]<-625
TSdata.cm$t_pgml[TSdata.cm$t_pgml == ">80000"]<-80000

TSdata.cm$Testosterone<-abs(as.numeric(TSdata.cm$t_pgml))/100 #pg/ml = pg/dl
TSdata.cm$Estradiol<-abs(as.numeric(TSdata.cm$E2_adj_conc_ngmL))#/1000 #ng/ml = pg/ml

#COMPILE TSIMANE DATASET
TSdata.in<-read.csv("BiochemFlo_copy.csv", stringsAsFactors = FALSE)

#merge with hormone data from AG
TSdata.1 <- full_join(TSdata.cm, TSdata.in)

#TSdata.cc <- TSdata.1 %>%
#  select(BiochemDate, Trimester, male, pid, Age)
#write.csv(TSdata.cc, "TSdata.cc.csv")

#remove old "sex" variable, depreciated for this dataset
TSdata.1$Sex<-NULL 
#create levels for sex vector, mutated to match coding values in NHANES dataset
#Change to 0 = male, 1 = female
TSdata.1$Sex[TSdata.1$male==1 | TSdata.1$Trimester %in% c("Male","Juvenile Male")]<-"Male"
TSdata.1$Sex[TSdata.1$male==0 & TSdata.1$Trimester %in% c("Lactating","Pre-Menst","Cycling","1","2","3","Menopause")]<-"Female"
TSdata.1$Sex<-factor(TSdata.1$Sex)

TSdata <- TSdata.1 %>%
  mutate(Age = abs(round(Age)), NEU = NeutroCnt*1000, 
         LYM = (WBC*Lymphocitos/100)*1000, MON = MonoCnt*1000, BAS = BasoCnt*1000, 
         EOS = EosinCnt*1000, CD4CD8 = CD48Ratio,
         NumPreg = NumGestas, MonthPreg=abs(as.integer(round(TSdata.1$PregDays/31))), 
         MonthPP=as.integer(round(IPI, digits=0)),
         lnTestosterone = log(Testosterone),
         lnEstradiol = log(Estradiol)) %>%
  mutate(WBC = 1000*WBC) %>% 
  select(Age:lnEstradiol, Testosterone, Estradiol, Sex, BMI, Trimester, NumPartos, crp, CD4Count, 
         CD8Count, BCount, WBC, NKCount, IgE, IgG, iga, igm, VSG, pid) %>%
  filter(!is.na(Age),!is.na(BMI))

TSdata$Preg[TSdata$Trimester %in% c("1","2","3")]<-"Yes"
TSdata$Preg[TSdata$Trimester %in% c("Pre-Menst","Cycling","Menopause") | TSdata$Sex=="Male"]<-"No"

##----------------------------------------------------------
#FIX NUMBER OF LIVE BIRTHS VARIABLE BY ADDING MISSED NULLIPAROUS INDIVIDUAL
#Give people who answered "NO" to "ever been pregnant?" a live birth value of zero
#people who have (1) never been pregnant or (2) never had a live birth == not currently breastfeeding
#people who have never been pregnant == not currently pregnant

TSdata$NumPartos[TSdata$Sex=="Male" | TSdata$Trimester == "Pre-Menst"]<-0
TSdata$NumPreg[TSdata$Sex=="Male" | TSdata$Trimester == "Pre-Menst"]<-0
#__________________________________________________________________
#Create functional age groups, based on female reproductive phases
TSdata$RepPhase <-  NULL
#Under the age of 8 or explicitly report that period hasn't started yet = pre-menses (0-11 years)
#no longer regularily cycling due to menopause = menopause (40-80)
TSdata$RepPhase[TSdata$Trimester == "Pre-Menst"]<-"Pre-menses"
TSdata$RepPhase[TSdata$Trimester == "Cycling" | 
                  TSdata$Trimester %in% c("1","2","3") | 
                  TSdata$Trimester == "Lactating"  | 
                  TSdata$MonthPP<=24] <- "Cyc-Preg-PP"
TSdata$RepPhase[TSdata$Trimester == "Menopause"]<-"Menopause"
TSdata$RepPhase[TSdata$Sex=="Male"]<-"Male"
table(TSdata$RepPhase)

#CREATE CYC-PREG-PP VARIABLE
TSdata$Group <- NULL
TSdata$Group[TSdata$Sex=="Male" & TSdata$Age >=10 & TSdata$Age <=50]<-"Male"
TSdata$Group[TSdata$NumPreg==0 & TSdata$Trimester=="Cycling"]<-"Nulliparous cycling"
TSdata$Group[TSdata$NumPreg>=1 & TSdata$Trimester=="Cycling"]<-"Parous cycling"
TSdata$Group[TSdata$Preg=="Yes" & TSdata$NumPreg==1]<-"Primiparous Pregnant"
TSdata$Group[TSdata$Preg=="Yes" & TSdata$NumPreg>1]<-"Multiparous Pregnant"
TSdata$Group[TSdata$MonthPP <= 24]<-"Postpartum"


#remove all RepPhase NAs
TSdataF <- TSdata %>%
  filter(!is.na(RepPhase), 
         !is.na(NumPartos), 
         !is.na(NumPreg),
         !is.na(Sex))

#DATA SPECIFIC DATAFRAME
TS.crp <- TSdataF %>%
  filter(!is.na(crp)) %>%
  mutate(Measure = "C-reactive Protein")

TS.wbc <- TSdataF %>%
  filter(!is.na(WBC),!is.na(LYM),
         !is.na(NEU),!is.na(EOS),
         !is.na(BAS),!is.na(MON)) %>%
  mutate(Measure = "Leukocyte differential")

TS.lym.dif <- TSdataF %>%
  filter(!is.na(CD4Count)) %>%
  mutate(Measure = "Lymphocyte differential")

TS.igs <- TSdataF %>% 
  filter(!is.na(IgE) & !is.na(IgG)) %>%
  mutate(Measure = "Immunoglobulins")

TS.esr <- TSdataF %>%
  filter(!is.na(VSG)) %>%
  mutate(Measure = "ESR")

TS.hrm <- TSdataF %>%
  filter(!is.na(Estradiol) & !is.na(Testosterone)) %>%
  mutate(Measure = "Hormones")

#put it back together
THLHP <- full_join(TS.lym.dif, TS.igs) %>%
  full_join(., TS.esr) %>%
  full_join(., TS.crp) %>%
  full_join(., TS.wbc) %>%
  full_join(., TS.hrm) %>%
  select(Age, Sex:Group, Measure) %>%
  mutate(Population = "THLHP",
         Sex = factor(Sex))
  

THLHP.sd <- THLHP %>% 
  select(Age, BMI, pid)
write.csv(THLHP.sd,'THLHP.sd.csv')

