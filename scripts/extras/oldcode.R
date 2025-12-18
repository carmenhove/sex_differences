table.names <- sort(c('DEMO', 'RHQ','BMX','MCQ', #Demographic, reproductive health, body mass data
                      paste0(c('DEMO','RHQ','BMX','MCQ'),"_",rep(c('B','C','D','E','F','G','H','I','J'),each=4)), 
                      'LAB25','L25_B','l25_2_b', 'L25_C', paste0('CBC',"_",c('D','E','F','G','H','I','J')), #CBC data
                      'LAB11','L11_B', 'L11_2_b','L11_C',paste0('CRP',"_",c('D','E','F')),
                      paste0('HSCRP',"_",c('I','J')), #CRP data
                      'TST_H','TST_I',paste0('SSCHL',"_",c('A','B','C')),
                      'AL_IGE_D'))

#apply nhanes() to extract SAS dataframes, transition to R list
nhanes.l1 <- sapply(table.names, nhanes) 

#remove SAS labels
nhanes.l2 <- sapply(nhanes.l1, remove_all_labels) 

nhanes.l2[c('HSCRP_I','HSCRP_J')] <- lapply(nhanes.l2[c('HSCRP_I','HSCRP_J')],
                                            function(x) x %>% mutate(LBXCRP = LBXHSCRP,Sensitivity = "High"))

#Fix unit discrepency bugs in hormone datafiles
##Testosterone, total (ng/dL), Estradiol (pg/mL)"
nhanes.l2[c('MCQ_B','MCQ_C','MCQ_D','MCQ_E','MCQ_F')] <- lapply(
  nhanes.l2[c('MCQ_B','MCQ_C','MCQ_D','MCQ_E','MCQ_F')], 
  function(x) rename(x, MCQ195 = MCQ190))

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
nhanes.l2[c('SSCHL_A','SSCHL_B','SSCHL_C')] <- lapply(
  nhanes.l2[c('SSCHL_A','SSCHL_B','SSCHL_C')], 
  function(x) mutate(x, LBXTST=SSTESTO*100,LBXEST = SSSE2))

#Reduce list, full join corresponding dataframes **find way to simplify this code!
nh.demo <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'DEMO')],full_join) #joining all demo files together
nh.cbc <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'CBC')],full_join)
nh.crp <- reduce(nhanes.l2[startsWith(names(nhanes.l2), c('L','CRP'))],full_join)
nh.rhq <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'RHQ')],full_join)
nh.bmi <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'BMX')], full_join)
#nh.horm <- reduce(nhanes.l2[startsWith(names(nhanes.l2), c('TS','SSCHL'))],full_join)
nh.med <- reduce(nhanes.l2[startsWith(names(nhanes.l2), 'MCQ')],full_join)
nh.ige <- as_tibble(nhanes.l2[['AL_IGE_D']]) %>% 
  select(SEQN, LBXIGE)

NHdata1<-full_join(nh.demo, nh.cbc) %>%
  full_join(., nh.crp) %>%
  full_join(., nh.rhq) %>%
  full_join(., nh.bmi) %>%
  full_join(., nh.ige) %>%
  full_join(., nh.med)

#Variable cleaning on combined dataframe
NHdata2 <- NHdata1 %>%
  mutate(Sex = RIAGENDR, BMI = BMXBMI, Height = BMXHT, Weight = BMXWT,
         Last.Period = case_when(RHQ050 == 1 | RHQ051 ==1 ~ "Now", 
                                 RHQ050 == 2 | RHQ051 ==2 ~ "2months"),
         Days.period = RHD080, #Number of days since the last period started.
         Tried.preg = RHQ074, DaysMenst = RHD080, MenstTime = RHQ051,
         IgE = LBXIGE,
         Age = RIDAGEYR, Preg.rhq = RHD143, 
         AFR =  replace(RHQ180, RHQ180 %in% c(777,999), NA),
         Preg.demo = RIDEXPRG, WBC = LBXWBCSI*1000, 
         NEU = LBDNENO*1000, LYM = LBDLYMNO*1000, 
         MON = LBDMONO*1000, BAS = LBDBANO*1000, 
         EOS = LBDEONO*1000, crp = LBXCRP*10, 
         pid = as.character(SEQN), 
         Reg.periods = RHQ031, EverPreg = RHQ131,
         BF = fct_recode(as.factor(RHQ200),"Yes"="1", "No"="2"),
         NumPreg = abs(RHQ160),  NumPartos = abs(RHQ171), 
         MonthPreg = abs(RHD152),  MonthPP = abs(RHQ197),
         #Estradiol = abs(LBXEST), Testosterone = abs(LBXTST),
         #lnEstradiol = log(Estradiol),  lnTestosterone = log(Testosterone),
         #EstTest = log(Estradiol/Testosterone),
         age.menarche = RHQ010, age.lastper = RHQ060, 
         noperiod.reason = RHD042, Menarche = MCQ149,
         # Clean up confusing coding in NumPreg, NumPartos, MonthPreg, MonthPP vectors
         ## Code 77 (refused), 99 (don't know) as NAs
         NumPartos = replace(NumPartos, NumPartos %in% c(77,99), NA),
         NumPreg = replace(NumPreg, NumPreg %in% c(77,99), NA),
         MonthPreg = replace(MonthPreg, MonthPreg %in% c(77,99,10), NA),
         MonthPP = replace(MonthPP, MonthPP %in% c(77,99), NA),
         Population = "NHANES") %>%
  select(Sex:Population) %>%
  filter(!is.na(Age) & !is.na(Sex))
