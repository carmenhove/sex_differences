

#set "menopause" answer to 7 across all years
RepHealth$RHD042[RepHealth$file %in% c("RHQ","RHQ_B") & RepHealth$RHD042==5]<-7

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


label(MedicalCond$SEQN) <- "Respondent sequence number"

##------------------------------
# MERGED NHANES INTIAL DATAFRAME
NHdata1<-full_join(Demographics, RepHealth) %>%
  full_join(., CBCdata) %>%
  full_join(., CRPdata) %>%
  full_join(., BMIdata) %>%
  full_join(., SexHormones) %>%
  full_join(., MedicalCond)
