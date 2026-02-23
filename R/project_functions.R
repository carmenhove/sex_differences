
#PROJECT FUNCTIONS

## Function to limit immune marker values to between population-pooled 1% and 99% percentiles
trim.values <- function(x, df){
  df2 <- df %>% filter(measure == unique(x$measure),Population == unique(x$Population))
  x2 <- x %>% filter(value <= df2$maximum & value >=c(df2$minimum)) 
  n_diff <- nrow(x) - nrow(x2)
  x2 <- x2 %>% mutate(removed = n_diff, original = nrow(x))
  x2
}

## Function to get matched samples across the THLHP and NHANES datasets by sex:reproductive phase and age
match.samples <- function(x){
  x.out <- matchit(as.factor(Population) ~ Age, 
                   data = x,
                   method = "nearest", 
                   exact = c("RepPhase","Age"))
  x.matched <- match.data(x.out)
  x.matched
}

## Function to run appropriate models
run.models <- function(x){
  # specify model formulas
  if(unique(x$Population) == "Tsimane" & unique(x$measure) != "NLR"){
    model_formula <- as.formula("as.integer(value) ~ s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase) + s(id, bs = 're')")
  } else if(unique(x$Population) == "Tsimane" & unique(x$measure) == "NLR"){
    model_formula <- as.formula("value ~ s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase) + s(id, bs = 're')")
  } else if(unique(x$Population)=="USA" & unique(x$measure)=="NLR"){
    model_formula <- as.formula("value ~ s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase)")
  } else {
    model_formula <- as.formula("as.integer(value) ~ s(Age, by = RepPhase) + BMI_zscore + s(NumPartos, by = RepPhase)")
  }
  # specify model family
  model_family <- "gaussian()"
  # set seed
  set.seed(1234)
  # run model
  model <- gam(model_formula, family = model_family, data = x, method = 'REML')
  model
}

## Get model statistics
get.statistics <- function(x, a, b, c){
  x <- x %>% 
    mutate(Term = str_c(Term, ":",`Sex:Reproductive Phase`)) %>% 
    filter(Population == a,
           Measure == b,
           Term == c)
  statistic <- str_c("(F = ", x$`F-statistic`, "; P-value = ", x$`P-value`,")")
  statistic
}


## Function to gather predicted values
get.predictions <- function(x, df){
  # ages by female reproductive phase
  female_df1 <- df %>%
    filter(Sex != "Male") %>% 
    select(RepPhase, Age) %>% 
    mutate(RepGroup = case_when(RepPhase %in% c("Cycling","Pregnant","Postpartum") ~ "Reproductive", TRUE ~ RepPhase),
           BMI_zscore = 0, id = 50) %>% 
    group_by(RepPhase) %>% 
    distinct(Age, .keep_all = T)
  # expand by parity (0 to 5)
  female_df2 <- data.frame(NumPartos=0:5) %>% 
    merge(female_df1, by=NULL) %>% 
    filter(!(RepPhase %in% c("Male","Pre-Menst") & NumPartos > 0), 
           !(RepPhase == "Postpartum" & NumPartos == 0)) %>% 
    select(RepPhase, RepGroup, id, BMI_zscore, Age, NumPartos, Sex)
  # male values
  male_df <- df %>% 
    filter(Sex == "Male") %>%
    distinct(Age, .keep_all = T) %>% 
    mutate(BMI_zscore = 0, id = 50) %>% 
    select(Sex, RepPhase, Age, BMI_zscore, id, NumPartos, Sex)
  # merge them all together
  new_data <- full_join(female_df2, male_df)
  
  ilink <- family(x)$linkinv
  set.seed(1234)
  pred <- predict(x, new_data, type = "link", se.fit = TRUE, exclude = "s(id)")
  pred <- cbind(pred, new_data)
  pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),
                    upr_ci = ilink(fit + (2 * se.fit)),
                    fitted = ilink(fit)) 
  pred <- pred  %>%
    mutate(RepPhase = ordered(
      RepPhase, levels = c("Male","Pre-Menst","Cycling",
                           "Pregnant","Postpartum","Menopause")))
  pred
}

## Get percent values
get.percent <- function(df,
                        ref_NumPartos,
                        new_NumPartos,
                        ref_RepPhase,
                        new_RepPhase,
                        ref_Age,
                        new_Age,
                        ref_Sex = "Female",
                        new_Sex) {
  
  # Extract the reference row
  ref_row <- df %>%
    filter(NumPartos == ref_NumPartos,
           RepPhase  == ref_RepPhase,
           Age == ref_Age,
           Sex == ref_Sex)
  
  # if (nrow(ref_row) == 0) {
  #   stop("Reference combination not found in dataframe.")
  # }
  
  if (nrow(ref_row) == 0) {
    return(NA_character_)
  }
  
  ref_value <- ref_row$fitted[1]
  ref_lwr   <- ref_row$lwr_ci[1]
  ref_upr   <- ref_row$upr_ci[1]
  
  # Compute percent differences relative to chosen reference
  pred <- df %>%
    mutate(
      ref_value = ref_value,
      ref_lwr   = ref_lwr,
      ref_upr   = ref_upr,
      pct_diff     = (fitted - ref_value) / ref_value * 100,
      pct_diff_lwr = (lwr_ci - ref_value) / ref_value * 100,
      pct_diff_upr = (upr_ci - ref_value) / ref_value * 100,
    ) %>% 
    filter(NumPartos == new_NumPartos, RepPhase == new_RepPhase, Age == new_Age, Sex == new_Sex) %>% 
    select(RepPhase, Age, NumPartos, Sex, starts_with("pct")) %>% 
    mutate(pct_diff = case_when(pct_diff_upr < 0 & pct_diff_lwr < 0 ~ gsub("-","",
                                                                           str_c(round(pct_diff, digits = 2), "% (95% CI: ", 
                                                                            round(pct_diff_upr, digits =2), "%, ", 
                                                                            round(pct_diff_lwr, digits = 2),"%)")
                                                                           ),
                                TRUE ~ str_c(round(pct_diff, digits = 2), "% (95% CI: ", 
                                             round(pct_diff_lwr, digits =2), "%, ", 
                                             round(pct_diff_upr, digits = 2),"%)")))
  pred$Age
  pred$pct_diff
}

## Function to get biases, separated by parity 
get.dfs <- function(x){
  x2 <- x %>%
    mutate(Bias = case_when(Male_lwr > upr_ci ~ "Male",
                            Male_upr < lwr_ci ~ "Female",
                            TRUE ~ "No Bias"),
           RepPhase = ordered(RepPhase,
                              levels = c("Male","Pre-Menst","Cycling","Pregnant",
                                         "Postpartum","Menopause")),
           Measure = ordered(Measure, levels = c("WBC","NEU","LYM","EOS","MON","NLR")),
           Sex = factor(Sex),
           Population = factor(Population)) %>%
    select(Population, Measure, fit, lwr_ci, upr_ci, Age, Sex, BMI_zscore,
           id, RepPhase, RepGroup, NumPartos,Unit, Bias, Male_median, Male_lwr, Male_upr) %>%
    arrange(Population, RepPhase, Measure, Age) %>%
    group_by(Population, Measure, RepPhase) %>%
    mutate(Group = cumsum(Bias != lag(Bias, default = Bias[[1]])) + 1)
  x2
}

## Get predictions by parity, controlling for age
get.predictions.parity <- function(x, df, age_rep = 24, age_meno = 65){
  # identify which population
  if(as.character(x$formula[[3]])[[3]] == "s(id, bs = \"re\")"){
    which_population = "Tsimane"
  }else{
    which_population = "USA"
  }
  
  new_data <- df %>%
    filter(Sex != "Male", RepPhase != "Pre-Menst") %>% 
    select(RepPhase, NumPartos, Population) %>% 
    mutate(RepGroup = case_when(RepPhase %in% c("Cycling","Pregnant","Postpartum") ~ "Reproductive",  TRUE ~ RepPhase),
           Age = case_when(RepGroup == "Reproductive" ~ age_rep,
                           RepPhase == "Menopause" ~ age_meno),
           BMI_zscore = 0,
           id = 50) %>%
    group_by(Population, RepPhase) %>% 
    distinct(NumPartos, .keep_all = T) %>% 
    select(RepPhase, RepGroup, id, BMI_zscore, Age, NumPartos, Population) %>% 
    filter(Population == which_population)
  
  ilink <- family(x)$linkinv
  set.seed(1234)
  pred <- predict(x, new_data, type = "link", se.fit = TRUE, exclude = "s(id)")
  pred <- cbind(pred, new_data)
  pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),
                    upr_ci = ilink(fit + (2 * se.fit)),
                    fitted = ilink(fit)) 
  pred <- pred  %>%
    mutate(RepPhase = ordered(
      RepPhase, levels = c("Cycling","Pregnant","Postpartum","Menopause")))
  pred
}


## Function to get Figure 1 plots
get.plot.repphase <- function(x, parity){
  x2 <- x %>% 
    filter(RepPhase %in% c("Male","Premenarchal") | NumPartos == parity)
    
  plot <- ggplot(x2, aes(x = Age, y =fit, color = RepPhase, fill = RepPhase)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.3, color = NA) +
    geom_line() +
    facet_grid(Measure ~ Population, scales = "free") +
    scale_fill_manual(
      values = c("#181818","#F28023", "#5A870A","#009BB0","#A99CD9","#B4450E")) + 
    scale_color_manual(
      values = c("#181818","#F28023","#5A870A","#009BB0","#A99CD9","#B4450E")) + 
    guides(fill=guide_legend(title="Sex:Reproductive \nPhase"),
           color =guide_legend(title="Sex:Reproductive \nPhase")) + 
    ylab(unique(x$Unit)) +
    xlab("Age (years)") + 
    expand_limits(y=0)
  plot
}

## Function to get Figure 2 plots
get.plot.parity <- function(x){
  
  x2 <- x %>% 
    filter(Age >= 14, Sex == "Female", NumPartos %in% c(1,5)) %>% 
    full_join(., x %>% filter(Sex == "Male", Age >= 14) %>% mutate(NumPartos = "1")) %>% 
    full_join(., x %>% filter(Sex == "Male", Age >= 14) %>% mutate(NumPartos = "5")) %>% 
    mutate(NumPartos = str_c(Population, ": ", NumPartos))
  
  plot <- ggplot(x2, aes(x = Age, y = log(fit), color = RepPhase, fill = RepPhase)) +
    geom_ribbon(aes(ymin = log(lwr_ci), ymax = log(upr_ci)), alpha = 0.3, color = NA) +
    geom_line() +
    facet_grid(Measure ~ NumPartos, scales = "free") +
    scale_fill_manual(
      values = c("#181818","#5A870A","#009BB0","#A99CD9","#B4450E")) + 
    scale_color_manual(
      values = c("#181818","#5A870A","#009BB0","#A99CD9","#B4450E")) + 
    guides(fill=guide_legend(title="Sex:Reproductive \nPhase"),
           color =guide_legend(title="Sex:Reproductive \nPhase")) + 
    ylab(str_c("Log ", unique(x$Unit))) + 
    expand_limits(y=0)
  plot
}

## Function to get Figure S3 plots
get.plot.parity.effect <- function(x){
  plot <- ggplot(x, aes(x = NumPartos, y = fit, color = RepPhase, fill = RepPhase)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.3, color = NA) +
    geom_line() +
    facet_grid(Measure ~ Population, scales = "free") + 
    scale_fill_manual(
      values = c("#5A870A","#009BB0","#A99CD9","#B4450E")) + 
    scale_color_manual(
      values = c("#5A870A","#009BB0","#A99CD9","#B4450E")) +
    xlab("Parity (live births)") + 
    ylab(unique(x$Unit)) + 
    guides(fill=guide_legend(title="Reproductive \nPhase"),
           color =guide_legend(title="Reproductive \nPhase"))+
    expand_limits(y=0)
  plot
}

## Function to get absolute values from Table 2 
get.values <- function(x, a, b, c, d=1){
    x2 <- x %>% 
      mutate(Population = case_when(Population == "Tsimane" ~ "THLHP", TRUE ~ "NHANES")) %>% 
      filter(Population == a,
             Measure == b,
             `Reproductive Phase` ==c) %>%
      separate(`Age Range`, c("Min","Max")) %>% 
      mutate(Group = row_number()) %>% 
      filter(Group == d)
  x2
}

## Function to get specified values from Table S3 for in-text references
get.abs.values <- function(x, a, b, c, d = 1){

  x2 <- x %>% 
    mutate(Population = case_when(Population == "Tsimane" ~ "THLHP", TRUE ~ "NHANES")) %>% 
    filter(Population == a, Measure == b, `Reproductive Phase` == c) %>% 
    mutate(`Female Value` = gsub("ratio","",`Female Value`),
           `Male Value` = gsub("ratio","",`Male Value`)) %>% 
    mutate(Group = row_number()) 
  
  if(max(x2$Group)==2){
    message("There are two groups. Default set to group 1.")
  }
  
  x3 <- x2 %>% filter(Group == d)
  x3

}

get.ci <- function(df, measure, population, parity, phase){
  
  df2 <- df %>% 
    filter(
      Measure == measure,
      RepPhase == phase,
      Population == population, 
      NumPartos == parity
    ) %>% 
    mutate(
      fit = case_when(
        as.character(.data$Unit) == "Cells/µL" ~ round(fit),
        TRUE ~ round(fit, 2)
      ),
      lwr_ci = case_when(
        as.character(.data$Unit) == "Cells/µL" ~ round(lwr_ci),
        TRUE ~ round(lwr_ci, 2)
      ),
      upr_ci = case_when(
        as.character(.data$Unit) == "Cells/µL" ~ round(upr_ci),
        TRUE ~ round(upr_ci, 2)
      ),
      text = case_when(
        as.character(.data$Unit) == "Cells/µL" ~ " cells/µL",
        TRUE ~ ""
      )
    ) %>% 
    mutate(
      ci = case_when(
        as.character(.data$Unit) == "Cells/µL" ~
          str_c(
            comma(fit), text,
            " (95% CI: ",
            comma(lwr_ci), "-",
            comma(upr_ci),
            ")"
          ),
        TRUE ~
          str_c(
            sprintf("%.2f", fit),
            " (95% CI: ",
            sprintf("%.2f", lwr_ci), "-",
            sprintf("%.2f", upr_ci),
            ")"
          )
      )
    )
  
  df2$ci
}

get.pct.diff <- function(df, Pop, Meas, RepPhase){
  df_new <- df %>% 
    filter(Population == Pop,
           Measure == Meas,
           `Reproductive Phase` == RepPhase)
  df_new$pct_diff
}


table_s3 %>% 
  filter(Population == "USA",
         Measure == "MON",
         `Reproductive Phase` == "Cycling") %>% 
  select(Diff_medians)

table_s3




