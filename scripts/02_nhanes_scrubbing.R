
#COMPILE NHANES DATASET
#extract NHANES tables and put them into list
datasets <- c('DEMO', 'RHQ', #Demographic and reproductive health data
              paste0(c('DEMO','RHQ'),"_",rep(c('B','C','D','E','F','G','H','I','J'),each=2)), 
              paste0('CBC',"_",c('D','E','F','G','H','I','J')), #CBC data
              'L11_B', 'L11_C', paste0('CRP',"_",c('D','E','F')), #CRP data
              paste0('BMX',"_",c('C','D','E','F','G','H','I')), #BMI data
              paste0('MCQ',"_",c('B','C','D')))#,'E','F','G','H','I','J','H','I')))

nhanes.l1 <- sapply(datasets, nhanes) #apply nhanes() to extract SAS dataframes, transition to R list
#nhanes("DEMO")
#remove SAS labels
nhanes.l2 <- sapply(nhanes.l1, remove_all_labels) 

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

#For MCQ_B/C/D/E/F MCQ195 needs to be recoded 
#nhanes.l2[c('MCQ_B','MCQ_C','MCQ_D','MCQ_E')]<-lapply(
#  nhanes.l2[c('MCQ_B','MCQ_C','MCQ_D','MCQ_E')], 
#  function(x) rename(x, MCQ195 = MCQ190))

#Fix unit discrepency bugs in hormone datafiles
##Testosterone, total (ng/dL), Estradiol (pg/mL)"
#nhanes.l2[c('SSCHL_A','SSCHL_B','SSCHL_C')] <- lapply(nhanes.l2[c('SSCHL_A','SSCHL_B','SSCHL_C')], 
#                                                      function(x) mutate(x, LBXTST=SSTESTO*100,LBXEST = SSSE2))


#Reduce list, full join corresponding dataframes 
nh.demo <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'DEMO')],full_join) %>%
  select(RIAGENDR, RIDAGEYR, RIDEXPRG, SEQN) #joining all demo files together

nh.cbc <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'CBC')],full_join) %>%
  select(SEQN, LBDNENO, LBDLYMNO, LBDBANO, LBDMONO, LBDEONO, LBXWBCSI)

nh.crp <- reduce(nhanes.l2[startsWith(names(nhanes.l2), c('L','CRP'))],full_join) %>%
  select(SEQN, LBXCRP)
  
nh.rhq <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'RHQ')],full_join) %>%
  select(SEQN, RHD143, RHQ031, RHQ131, RHQ160, RHQ200, RHQ171, RHQ197, RHD152, RHQ010,RHD042)

nh.bmi <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'BMX')], full_join) %>%
  select(SEQN, BMXBMI)
#nh.horm <- reduce(nhanes.l2[startsWith(names(nhanes.l2), c('TS','SSCHL'))],full_join)

nh.mcq <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'MCQ')], full_join) %>%
  select(SEQN, MCQ149)

#put all nested joins back together into dataframe
nhanes.df1 <- full_join(nh.cbc, nh.crp) %>%
  full_join(., nh.rhq) %>%
  full_join(., nh.bmi) %>%
  full_join(., nh.mcq) %>%
  full_join(., nh.demo)

#Variable cleaning on combined dataframe
nhanes.df2 <- nhanes.df1 %>%
  mutate(WBC = LBXWBCSI*1000, 
         NEU = LBDNENO*1000, LYM = LBDLYMNO*1000, 
         MON = LBDMONO*1000, BAS = LBDBANO*1000, 
         EOS = LBDEONO*1000, crp = LBXCRP*10) %>%
  rename(Preg.rhq = RHD143,
         Preg.demo = RIDEXPRG,
         Age = RIDAGEYR,
         Sex = RIAGENDR,
         BMI = BMXBMI,
         Menarche.rhq = RHQ010,
         Menarche.mcq = MCQ149,
         RegPeriods = RHQ031,
         ReasonsNoPeriods = RHD042,
         EverPreg = RHQ131,
         BF = RHQ200,
         NumPreg = RHQ160,
         NumPartos = RHQ171,
         MonthPP = RHQ197,
         MonthPreg = RHD152,
         pid = SEQN) %>%
  mutate(
         #Estradiol = abs(LBXEST), Testosterone = abs(LBXTST),
         #lnEstradiol = log(Estradiol),  lnTestosterone = log(Testosterone),
         #EstTest = log(Estradiol/Testosterone),
         # Clean up confusing coding in NumPreg, NumPartos, MonthPreg, MonthPP vectors
         ## Code 77 (refused), 99 (don't know) as NAs
         NumPartos = replace(NumPartos, NumPartos %in% c(77,99), NA),
         NumPreg = replace(NumPreg, NumPreg %in% c(77,99), NA),
         MonthPreg = replace(MonthPreg, MonthPreg %in% c(77,99,10), NA),
         MonthPP = replace(MonthPP, MonthPP %in% c(77,99), NA),
         Population = "USA") %>%
  mutate(Preg = case_when(Preg.demo == 1 | Preg.rhq ==1 | ReasonsNoPeriods == 1 ~ "Yes", #Yes, pregnant
                          Preg.demo == 2 | Preg.rhq == 2 | EverPreg == 2 ~ "No", #No, not pregnant
                          TRUE ~ NA_character_),
         Menarche = case_when(Menarche.rhq == 1 | Menarche.mcq == 1 ~ "Yes",
                              Menarche.rhq == 2 | Menarche.mcq == 2 ~ "No",
                              TRUE ~ NA_character_),
         Sex = case_when(Sex == 1 ~ "Male",
                         Sex == 2 ~ "Female"),
         RegPeriods = case_when(RegPeriods == 1 ~ "Yes",
                                RegPeriods == 2 ~ "No",
                                TRUE ~ NA_character_),
         BF = case_when(BF == 1 | ReasonsNoPeriods == 2 ~ "Yes",
                        BF == 2 ~ "No",
                        TRUE ~ NA_character_),
         Menopause = case_when(ReasonsNoPeriods == 7 ~ "Yes",
                               TRUE ~ NA_character_),
         EverPreg = case_when(EverPreg == 1 ~ "Yes",
                              EverPreg == 2 ~ "No",
                              TRUE ~ NA_character_),
         pid = as.character(pid)) %>%
  select(pid,Preg.rhq:Menopause) %>%
  filter(!is.na(Age), !is.na(Sex)) %>%
  select(-ends_with(".rhq"), -ends_with(".mcq"), -ends_with(".demo"))

str(nhanes.df2)

nhanes.df3 <- nhanes.df2 %>%
  mutate(RepPhase = case_when(Sex == "Male" ~ "Male",
                              Menarche == "No" | Age <= 8 ~ "Pre-Menst",
                              Preg == "Yes" | MonthPreg %in% c(0:10) ~ "Pregnant",
                              MonthPP <= 12 ~ "Postpartum",
                              RegPeriods == "Yes" ~ "Cycling",
                              Menopause == "Yes" ~ "Menopause",
                              TRUE ~ NA_character_)) %>%
  filter(!is.na(RepPhase) & (!is.na(WBC) | !is.na(crp))) %>%
  select(pid, Population, Age, BMI, NumPartos, RepPhase, Sex, MonthPP, MonthPreg,
         WBC, NEU, LYM, MON, EOS, BAS, crp)


table(nhanes.df3$RepPhase)

nhanes.df3 %>% 
  group_by(RepPhase) %>% 
  summarise(Age = str_c(median(Age, na.rm = T),
                        " (",min(Age, na.rm = T),"-",max(Age, na.rm = T),")"),
            N = length(pid),
            n = length(unique(pid)))
