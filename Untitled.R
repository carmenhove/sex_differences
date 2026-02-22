table_s3 %>% 
  mutate(`Reproductive Phase` = case_when(`Reproductive Phase` == "Pre-Menst" ~ "Premenarchal",
                                          `Reproductive Phase` == "Menopause" ~ "Postmenopausal",
                                          TRUE ~ `Reproductive Phase`),
         `Reproductive Phase` = ordered(`Reproductive Phase`,
                                        levels = c("Premenarchal","Cycling","Pregnant",
                                                   "Postpartum","Postmenopausal"))) %>%
  select(Population, Measure, `Reproductive Phase`, Age, NumPartos, Bias, `Female Value`, `Male Value`) %>% 
  rename(Parity = NumPartos) %>% 
  arrange(`Reproductive Phase`,Population,Measure)
