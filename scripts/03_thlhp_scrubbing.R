#THLHP

#COMPILE TSIMANE DATASET
thlhp.df1<-read.csv("./data/raw_data/BioChemFlo_copy.csv", 
                    stringsAsFactors = FALSE)

thlhp.df2 <- thlhp.df1 %>%
  mutate(Sex = case_when(male == 1 | Trimester %in% c("Male","Juvenile Male") ~ "Male",
                         male == 0 | Trimester %in% 
                           c("Lactating","Pre-Menst","Cycling","1","2","3","Menopause") ~ "Female"),
         Age = abs(round(Age)), 
         NEU = NeutroCnt*1000, 
         LYM = (WBC*Lymphocitos/100)*1000, 
         MON = MonoCnt*1000, 
         BAS = BasoCnt*1000, 
         EOS = EosinCnt*1000, 
         #CD4CD8 = CD48Ratio,
         NumPreg = NumGestas, 
         NumPartos =NumPartos,
         MonthPreg=abs(as.integer(round(PregDays/31))), 
         MonthPP=as.integer(round(IPI, digits=0))) %>% #,
         #lnTestosterone = log(Testosterone),
         #lnEstradiol = log(Estradiol)) %>%
  mutate(WBC = 1000*WBC,
         Preg = case_when(Trimester %in% c("1","2","3") ~ "Yes",
                          Trimester %in% c("Pre-Menst","Cycling","Menopause") ~ "No",
                          TRUE ~ NA_character_),
         Population = "Tsimane") %>% 
         # pid = str_c("TS",pid)) %>% 
  select(Sex, pid, Age, BMI, Trimester, WBC, crp, NumPartos, NEU:MonthPP, Population) %>%
  filter(!is.na(Age),!is.na(BMI) & (!is.na(WBC) | !is.na(crp)))


thlhp.df3 <- thlhp.df2 %>%
  mutate(RepPhase = case_when(Sex == "Male" ~ "Male",
                              Trimester == "Cycling" ~ "Cycling",
                              Trimester %in% c("1","2","3") ~ "Pregnant",
                              MonthPP <= 12 ~ "Postpartum",
                              Trimester == "Pre-Menst" ~ "Pre-Menst",
                              Trimester == "Menopause" ~ "Menopause")) %>%
  filter(!is.na(RepPhase)) %>%
  select(pid, Population, Age, BMI, NumPartos, RepPhase, Sex, MonthPP, MonthPreg,
         WBC, NEU, LYM, MON, EOS, BAS, crp)

thlhp.df3 %>% 
  group_by(RepPhase) %>% 
  summarise(Age = str_c(median(Age, na.rm = T),
                        " (",min(Age, na.rm = T),"-",max(Age, na.rm = T),")"),
            N = length(pid),
            n = length(unique(pid)),
            Repeats = N-n)



