
#PACKAGES
#PACKAGES
pkgs = c("nhanesA","tidyverse","sjlabelled","Hmisc","brms","MatchIt","optmatch",
         "tidybayes","magrittr","knitr","patchwork","ggpubr","scales")
install.packages(pkgs)
inst = lapply(pkgs, library, character.only = TRUE) 

if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots', force = TRUE)

library(raincloudplots)

#COMPILE NHANES DATASET
#extract NHANES tables and put them into list
table.names <- sort(c('DEMO', 'RHQ','BMX','MCQ', #Demographic, reproductive health, body mass data
                      paste0(c('DEMO','RHQ','BMX','MCQ'),"_",rep(c('B','C','D','E','F','G','H','I','J'),each=4)), 
                      'LAB25','L25_B','l25_2_b', 'L25_C', paste0('CBC',"_",c('D','E','F','G','H','I','J')), #CBC data
                      'LAB11','L11_B', 'L11_2_b','L11_C',paste0('CRP',"_",c('D','E','F')),
                      #paste0('HSCRP',"_",c('I','J')), #CRP data
                      #'TST_H','TST_I',paste0('SSCHL',"_",c('A','B','C')),
                      'AL_IGE_D'))

#apply nhanes() to extract SAS dataframes, transition to R list
nhanes.l1 <- sapply(table.names, nhanes) 

#remove SAS labels
nhanes.l2 <- sapply(nhanes.l1, remove_all_labels) 

#nhanes.l2[c('HSCRP_I','HSCRP_J')] <- lapply(nhanes.l2[c('HSCRP_I','HSCRP_J')],
#                                            function(x) x %>% mutate(LBXCRP = LBXHSCRP,
                                           #                          Sensitivity = "High"))

#Fix unit discrepency bugs in hormone datafiles
##Testosterone, total (ng/dL), Estradiol (pg/mL)"
#[c('MCQ_B','MCQ_C','MCQ_D','MCQ_E','MCQ_F')] <- lapply(
#  nhanes.l2[c('MCQ_B','MCQ_C','MCQ_D','MCQ_E','MCQ_F')], 
#  function(x) rename(x, MCQ195 = MCQ190))

#Fix RHQ datafile bugs/mismatched variable names
nhanes.l2[['RHQ']] <- nhanes.l2[['RHQ']] %>%
  rename(RHD042 = RHQ040, #reason no regular periods
         RHQ031 = RHQ030, #having regular periods
         RHD143 = RHQ140) %>% #do you think that you are pregnant now?
  #set "menopause" answer to 7 across all years
  mutate(RHD042 = replace(RHD042, RHD042 == 5, 7)) %>%
  select(-RHD470) 

nhanes.l2[['RHQ_B']] <- nhanes.l2[['RHQ_B']] %>%
  rename(RHD042 = RHQ040, #reason no regular periods
         RHQ031 = RHQ030, #having regular periods
         RHD143 = RHQ141) %>% #do you think that you are pregnant now?
  #set "menopause" answer to 7 across all years
  select(-RHD470)

#For RHQ_H/I/J, RHD042 needs to be recoded to "reason no regular periods"
nhanes.l2[c('RHQ_H','RHQ_I','RHQ_J')]<-lapply(nhanes.l2[c('RHQ_H','RHQ_I','RHQ_J')], 
                                              function(x) rename(x, RHD042=RHD043))

#Fix unit discrepency bugs in hormone datafiles
##Testosterone, total (ng/dL), Estradiol (pg/mL)"
#nhanes.l2[c('SSCHL_A','SSCHL_B','SSCHL_C')] <- lapply(
#  nhanes.l2[c('SSCHL_A','SSCHL_B','SSCHL_C')], 
#  function(x) mutate(x, LBXTST=SSTESTO*100,LBXEST = SSSE2))

#Reduce list, full join corresponding dataframes **find way to simplify this code!
nh.demo <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'DEMO')],full_join)
nh.cbc <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'CBC')],full_join)
#nh.crp <- reduce(nhanes.l2[c('LAB11','L11_B', 'L11_2_b','L11_C',
#                             'CRP_D','CRP_E','CRP_F')],full_join)
nh.rhq <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'RHQ')],full_join)
nh.bmi <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'BMX')], full_join) %>% 
  select(SEQN, BMXBMI, BMXHT, BMXWT)
#nh.horm <- reduce(nhanes.l2[startsWith(names(nhanes.l2), c('TS','SSCHL'))],full_join)
nh.med <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'MCQ')],full_join)

#nh.ige <- as_tibble(nhanes.l2[['AL_IGE_D']]) %>% 
#  select(SEQN, LBXIGE)

#Preliminary merged dataframe
nhanes.df1 <- full_join(nh.demo, nh.bmi) %>%
  full_join(., nh.rhq) %>%
  full_join(., nh.cbc) %>%
  #full_join(., nh.crp) %>% #filter(Sensitivity != "High")) %>%
  #full_join(., nh.horm) %>% 
  #full_join(., nh.ige) %>% 
  full_join(., nh.med)

#Variable cleaning on combined dataframe
NHANES <- nhanes.df1 %>%
  rename(Sex = RIAGENDR, Age = RIDAGEYR, BMI = BMXBMI, 
         Height = BMXHT, Weight = BMXWT,
         Preg.demo = RIDEXPRG, Preg.rhq = RHD143, 
         EverPreg = RHQ131, NumPreg = RHQ160, NumBirths = RHQ171, 
         MonthPreg = RHD152,  MonthPP = RHQ197, BFnow = RHQ200,
        # Estradiol = LBXEST, Testosterone = LBXTST,
         WBC = LBXWBCSI, NEU = LBDNENO, LYM = LBDLYMNO, 
         MON = LBDMONO, BAS = LBDBANO, EOS = LBDEONO,
        #IgE = LBXIGE, 
        age.menarche = RHQ010, age.lastper = RHQ060,
        noperiod.reason = RHD042, Menarche = MCQ149) %>%
  mutate(Year.PP = case_when(Age - RHQ190 <=1 | MonthPP <=12 ~ "Yes",TRUE ~ "No"),
         NLR = NEU/LYM, 
         #Regular period = report period within last 2 months
         #RHD080 = # of days since last period started (RHQ:RHQ_D) (days as integer)
         #RHQ051 = When did you have your last period? RHQ_C:RHQ_D (1 = now, 2 = less than 2 months)
         #RHQ050 = when did you have last period? RHQ:RHQ_B (1 = now, 2 = less than 2 months)
         #RHQ031 = Regular period in last 12 months
         AFR =  replace(RHQ180, RHQ180 %in% c(777,999), NA),
         #Menarche = MCQ149,
         NumPartos = case_when(NumBirths > 20 ~ NA_integer_,
                               TRUE ~ NumBirths),
         Reg.Periods = case_when(RHQ051 %in% c(1,2) | RHQ050 %in% c(1,2) | RHD080 <=60 ~ 1, TRUE ~ 2),
         Preg = case_when(Preg.demo ==1 | Preg.rhq==1 | MonthPreg %in% c(1:10) ~ 1, #Pregnant
                          TRUE ~ 2),
                          #Preg.demo ==2 | Preg.rhq==2 | EverPreg == 2 | 
                          #  Menarche == 2 | age.menarche == 0 ~ 0,
                          #Sex  == 1  ~ 3),#Not pregnant
                              #TRUE ~ 999),
         #BF = case_when(EverPreg == 2 | NumBirths == 0 | NumPreg == 0 | BFnow == 2 ~ 2,
         #               BFnow == 1 ~ 1, TRUE ~ 999),
         #Sensitivity = case_when(Sensitivity == "High" ~ "High",TRUE ~ "Normal"),
         #Exam = case_when(Exam == "Second" ~ "Second",TRUE ~ "First"),
         across(c(MonthPreg, MonthPP),abs),
         across(c(WBC,NEU,LYM,EOS,MON,BAS), ~ .x*1000),
         #crp = LBXCRP*10, 
         Population = "NHANES", pid = as.character(SEQN),
         RepGroup = case_when(Sex == 1 ~ "Male",
                              Menarche == 2 | age.menarche == 0 | (Sex == 2 & Age < 8) ~ "Pre-menarche",
                              Reg.Periods == 1 & Preg == 2 & Year.PP == "No" ~ "Cycling",
                              Preg == 1 | MonthPreg <=10 ~ "Pregnant",
                              MonthPP <=12 & Preg == 2 ~ "Postpartum",
                              #| (!is.na(IgE) & Year.PP == "Yes" & Preg ==2) ~ "Postpartum",
                              Sex == 2 & noperiod.reason == 7 ~ "Menopause")) %>% 
  filter(!is.na(Sex) & !is.na(RepGroup) &
           !(RepGroup == "Pre-menarche" & Age > 13) &
           !(RepGroup == "Cycling" & Age > 52) &
           !(RepGroup == "Pregnant" & Age > 41) &
           !(RepGroup == "Postpartum" & Age > 42) &
           !(RepGroup == "Menopause" & Age < 39)) 

#quantile(NHANES[NHANES$RepGroup=="Pre-menarche",]$Age, probs = 0.99) #13
#quantile(NHANES[NHANES$RepGroup=="Cycling",]$Age, probs = 0.99) #52 
#quantile(NHANES[NHANES$RepGroup=="Pregnant",]$Age, probs = 0.99) #41 
#quantile(NHANES[NHANES$RepGroup=="Postpartum",]$Age, probs = 0.99) #42
#quantile(THLHP[THLHP$RepGroup=="Menopause",]$Age, probs = c(0.01,0.99)) #39 - 85

NHANES %>% 
  group_by(RepGroup) %>% 
  summarise(Age = get.dezies(Age))

##-------------------------------------------------------
##THLHP
#HORMONE THLHP DATASET
TSdata.cm <- read.csv("cm_data.csv", stringsAsFactors = FALSE) %>%
  mutate(t_pgml = case_when(t_pgml == "<625" | t_pgml == "<625.36" ~ 625,
                            t_pgml == ">80000" ~ 80000),
         Testosterone = abs(as.numeric(t_pgml))/100,
         Estradiol = abs(as.numeric(E2_adj_conc_ngmL)))

#COMPILE TSIMANE DATASET
TSdata.in<-read.csv("BiochemFlo_copy.csv", stringsAsFactors = FALSE)

#merge with hormone data from AG
TSdata1 <- full_join(TSdata.cm, TSdata.in) %>%
  mutate(Sex = case_when(male == 1 | Trimester %in% c("Male","Juvenile Male") ~ 1,
                         male==0 & Trimester %in% c("Lactating","Pre-Menst",
                                                    "Cycling","1","2","3","Menopause") ~ 2),
         Age = abs(round(Age)), NEU = NeutroCnt*1000, 
         LYM = (WBC*Lymphocitos/100)*1000, MON = MonoCnt*1000, BAS = BasoCnt*1000, 
         EOS = EosinCnt*1000, CD4CD8 = CD48Ratio,
         NumPreg = NumGestas, MonthPreg=trunc(PregDays/31), 
         MonthPP=as.integer(round(IPI, digits=0)),
         lnTestosterone = log(Testosterone),
         lnEstradiol = log(Estradiol),
         Hookworm = as.factor(Present.Hookworm),
         Roundworm = as.factor(Present.A.lumbricoides),
         Strongiloides = as.factor(Present.Strongiloides)) %>%
  mutate(WBC = 1000*WBC, NLR = NEU/LYM) %>% 
  select(Sex, Age, NEU, NLR, LYM, EOS, MON, BAS, CD4CD8, NumPreg, MonthPreg, MonthPP,
          lnTestosterone, lnEstradiol, Hookworm, Roundworm, Strongiloides, 
         Testosterone, Estradiol, BMI, 
         Trimester, NumPartos, crp, CD4Count, 
         CD8Count, BCount, WBC, NKCount, IgE, IgG, 
         iga, igm, VSG, pid, Height, Weight, comunid) %>%
  filter(!is.na(Age),!is.na(Sex))

THLHP <- TSdata1 %>%
  mutate(RepGroup = case_when(Sex == 1 | Trimester %in% c("Male","Juvenile Male") ~ "Male",
                              Trimester == "Pre-Menst" ~ "Pre-menarche",
                              Trimester == "Cycling" & Age >= 8 ~ "Cycling",
                              Trimester %in% c("1","2","3") & MonthPreg <=10 ~ "Pregnant",
                              MonthPP <= 12 & Trimester != "1" & Trimester != "2" & 
                                Trimester != "3" ~ "Postpartum",
                              Trimester == "Menopause" & Age > 30 ~ "Menopause"),
         Population = "THLHP",
         NumPartos = case_when(NumPartos > 20 ~ NA_integer_,
                               TRUE ~ NumPartos)) %>%
  filter(!is.na(Sex) & !is.na(Age) & !is.na(RepGroup))

THLHP %>% 
   group_by(RepGroup) %>% 
  summarise(Age = get.dezies(Age))
  
##PUT TOGETHER
MERGED <- full_join(THLHP,NHANES) %>%
  mutate(Sex = fct_recode(as.factor(Sex),"Male"="1", "Female"="2"),
         RepGroup = factor(RepGroup, 
                           levels = c("Male","Pre-menarche","Cycling",
                                      "Pregnant","Postpartum","Menopause")),
         RepGroup.2 = case_when(RepGroup 
                                %in% c("Cycling","Pregnant","Postpartum") ~ "Reproductive",
                                RepGroup == "Menopause" ~ "Post-Reproductive",
                                RepGroup == "Pre-menarche" ~ "Pre-pubertal",
                                RepGroup == "Male" ~ "Male")) %>% 
  mutate(lnWBC = log(WBC), lnNEU = log(NEU),
         lnLYM = log(LYM), lnNLR = log(NLR),
         lnCRP = log(crp), lnIgE = log(IgE)) %>% 
         #MonthPP = as.double(MonthPP), MonthPreg = as.double(MonthPreg),
         #MonthPP.adj = case_when(RepGroup == "Postpartum" ~ MonthPP, TRUE ~ as.integer(0)),
         #MonthPreg.adj = case_when(RepGroup == "Pregnant" ~ MonthPreg, TRUE ~ as.double(0)),
         #Parity = case_when(Sex == "Female" & NumPartos < 20 ~ NumPartos,TRUE ~ as.double(0)),
         #Pregnancies = case_when(Sex == "Female" & NumPreg > 0 ~ NumPreg, TRUE ~ as.integer(0))) %>% 
  select(Sex, Population, Age, BMI, RepGroup, RepGroup.2,
         WBC, NEU, LYM, EOS, MON, BAS, crp, IgE,NLR,pid,
         MonthPP, MonthPreg, NumPartos, NumPreg, Height, Weight) %>% 
  filter(!is.na(Sex) & !is.na(Age) & !is.na(BMI) & 
           !is.na(WBC) & !is.na(NEU) & !is.na(LYM) &  
           !is.na(EOS) & !is.na(MON) & !is.na(BAS) & NLR != "Inf")
  
#  filter(!is.na(WBC) & !is.na(NEU) & !is.na(LYM) &  
#           !is.na(EOS) & !is.na(MON) & !is.na(BAS)) %>%
#  mutate(Measure = "Leukocyte Differential")

#C-reactive Protein
#crp <- MERGED %>% 
#  filter(!is.na(crp)) %>% 
#  mutate(Measure = "C-reactive Protein")

#Antibody differential
#antibodies <- MERGED %>% 
#  filter(!is.na(IgE)) %>% 
 # mutate(Measure = "IgE")

#FINAL DATAFRAME
#FINAL.1 <- full_join(wbc.diff, crp) %>%
 # full_join(., antibodies) 

#Get age-matched samples within each Population dataset
#Shared <- FINAL.1 %>% 
 # filter(Measure %in% c("Leukocyte Differential", 
 #                       "C-reactive Protein","Antibodies"))

list.shared <- split(MERGED, list(MERGED$RepGroup.2, MERGED$Population))#, 
                                  #Shared$Measure))

list1 <- list.shared #Males and females
list2 <- list.shared[startsWith(names(list.shared), "Male") == FALSE] #Females only

get.matched <- function(x){ 
  population <- as.character(unique(x$Population)) #Population e.g. NHANES
  #measure <- as.character(unique(x$Measure)) #Measure e.g., C-reactive Protein
  subset <- paste0("Male",".", population)# ,".",measure) 
  #Male subset e.g, C-reactive Protein
  males <- as.data.frame(list1[[subset]])
  merged <- full_join(males, as.data.frame(x)) %>%
    mutate(category = as.character(unique(x$RepGroup.2)))#,
           #Measure = as.character(unique(x$Measure)))
  match.it <- matchit(Sex ~ Age,
                      data = merged,
                      method = "nearest",
                      ratio = 1)
  match.df <- match.data(match.it)[1:ncol(x)] 
  match.df
}

matched.list <- map(list2, get.matched)

matched.df <- plyr::ldply(matched.list,data.frame, .id="Category")  %>% 
  mutate(Repcat = gsub("\\..*", "", Category),
         Repcat2 = str_c(Sex, RepGroup),
         NumPartos = case_when(Sex == "Male" ~ NA_integer_,
                               TRUE ~ NumPartos)) #%>% 
  #mutate(Repcat = ordered(Repcat, levels = c("Pre-pubertal",
  #                                           "Reproductive",
  #                                           "Post-Reproductive")))


