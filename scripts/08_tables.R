
#TABLES

## PRIMARY TABLES
table_1 <- data.frame(
  Measure = c("Total white blood cells", 
              "Neutrophils", 
              "Total lymphocytes",
              "Eosinophils",
              "Monocytes",
              "Neutrophil-Lymphocyte Ratio"),#,
             # "Basophils"),
  Abbreviation = c("WBC","NEU","LYM","EOS","MON","NLR"),#"BAS"),
  Description = c("The total number of circulating white blood cells (leukocytes).",
                  "Most abundant type of white blood cell involved in myriad immune processes (e.g., antigen presentation, phagocytosis, priming of antigen-specific immune response). Implicated in autoimmune disease [@wigerblad_kaplan23].",
                  "White blood cells characterized by the presence of the CD45 receptor and responsible for generating antigen-specific immune responses. Composed of T cells, B cells, and natural killer cells.",
                  "Granulocytes that defend against macroparasites and contribute to allergic responses.",
                  "Phagocytic white blood cells that migrate to sites of infection and injury where they differentiate into macrophages.", 
                  "General marker of immune system homeostasis and the balance between innate and antigen-specific immunity. Values above 3 are generally considered to indicate inflammation [@kourilovitch_galarza-maldonado23]. Predictive of all-cause and cardiovascular mortality among US adults with rheumatoid arthritis [@zhou_etal23] and presence/severity of preeclampsia during pregnancy [@mohamed_ali23], multiple sclerosis [@damico_etal19; @hasselbalch_etal18], primary Sjögren's syndrome [@mihai_etal22], thyroid cancer [@cheong_etal21], and systemic lupus erythematosus [@wang_etal20].")) #,
                  #"Granulocytes that contribute to defense against macroparasites and allergic responses and mediate production of histamines."))#,

# Table 2
table2_df <- map(pred_parity_list, get.dfs) %>% 
  bind_rows(., .id = "NumPartos")

table_2 <- table2_df %>% 
  filter(NumPartos ==3 | Sex == "Male" | RepPhase == "Pre-Menst") %>% 
  group_by(RepPhase) %>% 
  mutate(`Age Span` = (max(Age) - min(Age))+1) %>% 
  group_by(Population, Measure, RepPhase, Group) %>% 
  summarise(`Sex Bias` = unique(Bias),
            `Age Range` = str_c(min(Age),"-",max(Age)),
            `Cumulative Years` = (max(Age) - min(Age))+1,
            `Age Span` = unique(`Age Span`),
            `% Age Span` = str_c(round(`Cumulative Years`/`Age Span`*100,digits = 2), "%")) %>% 
  filter(`Sex Bias` != "No Bias") %>% 
  arrange(Population, RepPhase, Measure) %>% 
  rename(`Reproductive Phase` = RepPhase)

## SUPPLEMENTARY TABLES

# Table S1
table_s1 <- model_matcheddf %>%
  group_by(RepPhase, Measure, Population) %>%
  tally() %>%
  pivot_wider(names_from = RepPhase, values_from = n) %>%
  mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM","NLR","MON","EOS"))) %>% 
  arrange(Measure, Population) %>% 
  select(Measure, Population, Male, `Pre-Menst`, Cycling, Pregnant, Postpartum, Menopause)

# Table S2
table_s2 <- model_matcheddf %>%
  filter(Population == "Tsimane") %>% 
  group_by(RepPhase, Measure) %>%
  summarise(value = str_c(length(unique(id)),  
                          " (", round(length(unique(id))/length(id) * 100, digits = 2),"%)")) %>% 
  pivot_wider(names_from = RepPhase, values_from = value) %>%
  mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM","NLR","MON","EOS"))) %>% 
  arrange(Measure) %>% 
  select(Measure, Male, `Pre-Menst`, Cycling, Pregnant, Postpartum, Menopause)

# #Table S3
# table_s3 <- pred_df %>% 
#   filter((RepPhase != "Pre-Menst" & NumPartos == 3) | RepPhase == "Pre-Menst") %>% 
#   mutate(Bias = case_when(Male_lwr > upr_ci ~ "Male",
#                    Male_upr < lwr_ci ~ "Female",
#                    TRUE ~ "No Bias")) %>% 
#   group_by(Population, Measure, RepPhase, Bias) %>% 
#   mutate(Diff_medians = abs(Male_median - fit)) %>% 
#   filter(Sex != "Male", Bias != "No Bias", Diff_medians == max(Diff_medians)) %>% 
#   mutate(`Female Value` = str_c(fit, " ", tolower(Unit), " (95% CI: ", lwr_ci,"-", upr_ci,")"),
#          `Male Value` = str_c(Male_median, " ", tolower(Unit), " (95% CI: ", Male_lwr,"-", Male_upr,")"),
#          Measure = ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON","NLR"))) %>% 
#   rename(`Reproductive Phase` = RepPhase) %>% 
#   arrange(`Reproductive Phase`, Population) %>% 
#   mutate(key = str_c(Population, ".", Measure)) %>% 
#   rowwise() %>% 
#   mutate(
#     pct_diff = if (key %in% names(pred_list)) {
#       get.percent(
#         pred_list[[key]],
#         0,
#         NumPartos,
#         "Male",
#         `Reproductive Phase`,
#         Age,
#         Age,
#         "Male",
#         "Female"
#       )
#     } else {
#       NA_character_
#     }
#   ) %>%
#   ungroup()

## Table S3
table_s3 <- pred_df %>% 
  filter((RepPhase != "Pre-Menst" & NumPartos == 3) | RepPhase == "Pre-Menst") %>% 
  mutate(Bias = case_when(
    Male_lwr > upr_ci ~ "Male",
    Male_upr < lwr_ci ~ "Female",
    TRUE ~ "No Bias"
  )) %>% 
  group_by(Population, Measure, RepPhase, Bias) %>% 
  mutate(Diff_medians = abs(Male_median - fit)) %>% 
  filter(Sex != "Male", Bias != "No Bias") %>%
  arrange(desc(Diff_medians), Age) %>%
  slice(1) %>%
  mutate(
    `Female Value` = str_c(fit, " ", tolower(Unit), 
                           " (95% CI: ", lwr_ci,"-", upr_ci,")"),
    `Male Value` = str_c(Male_median, " ", tolower(Unit), 
                         " (95% CI: ", Male_lwr,"-", Male_upr,")"),
    Measure = ordered(Measure, 
                      levels = c("WBC","NEU","LYM","EOS","MON","NLR"))
  ) %>% 
  rename(`Reproductive Phase` = RepPhase) %>% 
  arrange(`Reproductive Phase`, Population) %>% 
  mutate(key = str_c(Population, ".", Measure)) %>% 
  rowwise() %>% 
  mutate(
    pct_diff = if (key %in% names(pred_list)) {
      get.percent(
        pred_list[[key]],
        0,
        NumPartos,
        "Male",
        `Reproductive Phase`,
        Age,
        Age,
        "Male",
        "Female"
      )
    } else {
      NA_character_
    }
  ) %>%
  ungroup()

## Model summaries
model_sum_list <- map(model_outputs, tidy)

model_summaries <- bind_rows(model_sum_list, .id = "Population.Measure") %>%
  separate(Population.Measure, c("Population","Measure")) %>% 
  mutate(term = gsub("RepPhase","",term),
         term = gsub("NumPartos","Parity",term),
         term = gsub("Pre-Menst","Premenarchal",term),
         term = gsub("Menopause","Postmenopausal",term)) %>% 
  rename(Term = term, EDF = edf, `F-statistic` = statistic, `P-value` = p.value) %>% 
  select(-ref.df) %>% 
  filter(!(Term %in% c("s(id)","s(Parity):Male", "s(Parity):Premenarchal"))) %>% 
  separate(Term, c("Term","RepPhase"), sep = ":") %>% 
  mutate(EDF = format(round(EDF, digits = 3), nsmall = 3),
         `F-statistic` = format(round(`F-statistic`, digits = 3),nsmall = 3),
         `P-value` = case_when(`P-value` < 0.001 ~ "<0.001",
                               TRUE ~ format(round(`P-value`, digits = 3),nsmall = 3)),
         Measure = ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON","NLR")),
         `Sex:Reproductive Phase` = ordered(RepPhase, levels = c("Male","Premenarchal", "Cycling",
                                         "Pregnant","Postpartum","Postmenopausal"))) %>% 
  arrange(Population, Measure, Term) #%>% 
 # unite(`Sex:Reproductive Phase`, c("Term","RepPhase"), sep = ":")

# 
# fixef_list <- map(model_outputs, ~ tidy(., parametric = T))
# 
# bind_rows(fixef_list, .id = "Population.Measure") %>%
#    separate(Population.Measure, c("Population","Measure")) %>%
#    mutate(term = gsub("RepPhase","",term),
#           term = gsub("NumPartos","Parity",term),
#           term = gsub("Pre-Menst","Premenarchal",term),
#           term = gsub("Menopause","Postmenopausal",term),
#           term = gsub("\\(Intercept\\)","Cycling (Intercept)",term)) %>%
#    rename(Term = term, Estimate = estimate, `Statistic` = statistic, `P-value` = p.value, `Std Error` = std.error) %>%
#    filter(!Term %in% c("BMI_zscore","Male")) %>%
#    mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON","NLR")),
#           Term = ordered(Term, levels = c("Premenarchal","Cycling (Intercept)","Pregnant",
#                                  "Postpartum","Postmenopausal"))) %>%
#    arrange(Measure,Population,Term)

age_deltas <- pred_list[[1]] %>% 
  filter(Sex == "Female") %>% 
  group_by(RepPhase) %>% 
  mutate(
    min_age = min(Age),
    max_age = max(Age),
    mid_age = (min(Age, na.rm = TRUE) + max(Age, na.rm = TRUE)) / 2,
    dist    = abs(Age - mid_age)
  ) %>%
  slice_min(dist, n = 1, with_ties = FALSE)

save.image('./Rmd/pnas.RData')
