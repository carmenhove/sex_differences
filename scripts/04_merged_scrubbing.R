
merged_df_unclean <-  full_join(thlhp.df3, nhanes.df3) %>%
  mutate(NLR = NEU/LYM,
         ARM = case_when(RepPhase == "Cycling" ~ -1,
                         RepPhase == "Pregnant" ~ MonthPreg,
                         RepPhase == "Postpartum" ~ MonthPP + 10,
                         TRUE ~ NA_real_),
         NumPartos = case_when(RepPhase == "Male" ~ NA_real_,
                               RepPhase == "Pre-Menst" & (NumPartos == 0 | is.na(NumPartos)) ~ 0,
                               RepPhase %in% c("Cycling","Pregnant","Postpartum","Menopause") ~ NumPartos,
                               TRUE ~ NA_real_))

early_meno <- merged_df_unclean %>% filter((RepPhase == "Menopause" & Age < 40)) %>% group_by(Population) %>% tally()
late_meno <- merged_df_unclean %>% filter((RepPhase == "Cycling" & Age > 70)) %>% group_by(Population) %>% tally()

merged_df <- merged_df_unclean %>% 
  group_by(pid) %>%
  pivot_longer(WBC:NLR, names_to = "measure") %>%
  filter(!is.na(value),
         (is.na(NumPartos) | NumPartos < 25),
         !(RepPhase == "Menopause" & Age < 40),
         !(RepPhase == "Cycling" & Age > 70)) %>%
  group_by(pid) %>%
  mutate(id=cur_group_id())

write.csv(merged_df,"/Users/carmenhove/Desktop/Grad School/PROJECTS/ARM/data/merged_df.csv")
