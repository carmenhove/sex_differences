#PLOTTING

## Figure 1
plot_df <- pred_parity_df %>% 
  mutate(RepPhase = case_when(RepPhase == "Pre-Menst" ~ "Premenarchal",
                              RepPhase == "Menopause" ~ "Postmenopausal",
                              TRUE ~ RepPhase),
         RepPhase = ordered(RepPhase,
                            levels = c("Male","Premenarchal","Cycling","Pregnant",
                                       "Postpartum","Postmenopausal")))

plot_list <- split(plot_df, list(plot_df$Population, plot_df$Unit))

figure_1a <- get.plot.repphase(plot_list[["USA.Cells/µL"]], 3)
figure_1b <- get.plot.repphase(plot_list[["Tsimane.Cells/µL"]], 3)
figure_1c <- get.plot.repphase(plot_list[["USA.Ratio"]], 3) + 
  scale_y_continuous(limits = c(0, 3.5))
figure_1d <- get.plot.repphase(plot_list[["Tsimane.Ratio"]], 3) + 
  scale_y_continuous(limits = c(0, 3.5))

figure_1 <- (figure_1a + figure_1b) / (figure_1c + figure_1d) +
  plot_layout(guides = "collect", heights = c(5,1)) +
  plot_annotation(tag_levels = "A") 

ggsave("./output/figure_1.png",figure_1,
       device = ragg::agg_png,dpi = 900,
       width = 18, height = 22, units = "cm")

# Figure 2
plot_df2 <- parity_effects_df %>% 
  mutate(RepPhase = case_when(RepPhase == "Menopause" ~ "Postmenopausal", TRUE ~ RepPhase),
         RepPhase = ordered(RepPhase,
                            levels = c("Male","Premenarchal","Cycling","Pregnant",
                                       "Postpartum","Postmenopausal")))

parity_plot_list <- split(plot_df2, list(plot_df2$Population, plot_df2$Unit))

figure_2a <- get.plot.parity.effect(parity_plot_list[["USA.Cells/µL"]])
figure_2b <- get.plot.parity.effect(parity_plot_list[["Tsimane.Cells/µL"]])
figure_2c <- get.plot.parity.effect(parity_plot_list[["USA.Ratio"]])
figure_2d <- get.plot.parity.effect(parity_plot_list[["Tsimane.Ratio"]]) 

figure_2 <- (figure_2a + figure_2b) / (figure_2c + figure_2d) +
  plot_layout(guides = "collect", heights = c(5,1)) +
  plot_annotation(tag_levels = "A") 

ggsave("./output/figure_2.png",figure_2,
       device = ragg::agg_png,dpi = 900,
       width = 18, height = 22, units = "cm")

# figure_2a <- get.plot.parity(plot_list[["USA.Cells/µL"]])
# figure_2b <- get.plot.parity(plot_list[["Tsimane.Cells/µL"]])
# figure_2c <- get.plot.parity(plot_list[["USA.Ratio"]]) + 
#   scale_y_continuous(limits = c(0.5, 4.25))
# figure_2d <- get.plot.parity(plot_list[["Tsimane.Ratio"]]) + 
#   scale_y_continuous(limits = c(0.5, 4.25))
# 
# figure_2 <- (figure_2a + figure_2b) / (figure_2c + figure_2d) +
#   plot_layout(guides = "collect", heights = c(5,1)) +
#   plot_annotation(tag_levels = "A") 
# 
# ggsave("./output/figure_2.png",figure_2,
#        device = ragg::agg_png,dpi = 600,
#        width = 11, height = 8.5, units = "in")

#par(mfrow = c(2, 2))
#gam.check(model_outputs[["USA.WBC"]][[1]])

##------------------------------------------------------------------------------
## SUPPLEMENTARY FIGURES

## Figure S1
figure_s1 <- ggplot(model_matcheddf %>% 
                      filter(Measure == "WBC"), 
                    aes(x = Age, y = log(BMI),
                        color = Sex)) +
  geom_point(position = "jitter", alpha = 0.6) +
  geom_vline(xintercept = 20, color = "darkgrey") + 
  facet_grid(Sex ~ Population) +
  ylab("Log BMI") + 
  xlab("Age (years)") + 
  scale_fill_manual(values = c("#B4450E","#181818"))+
  scale_color_manual(values = c("#B4450E","#181818"))

ggsave("./output/figure_s1.png",figure_s1,
       device = ragg::agg_png,dpi = 900,
       width = 11, height = 8, units = "cm")

## Figure S2
figure_s2 <- ggplot(model_matcheddf %>% 
                      mutate(RepPhase = case_when(RepPhase == "Pre-Menst" ~ "Premenarchal",
                                                  RepPhase == "Menopause" ~ "Postmenopausal",
                                                  TRUE ~ RepPhase),
                             RepPhase = ordered(RepPhase,
                                                levels = c("Male","Premenarchal","Cycling","Pregnant",
                                                           "Postpartum","Postmenopausal"))), 
                    aes(x = Age, fill = RepPhase)) + 
  geom_histogram(alpha = 0.7) + 
  facet_grid(Population ~ Measure, scales = "free") + 
  guides(fill=guide_legend(title="Sex:Reproductive \nPhase"),
         color =guide_legend(title="Sex:Reproductive \nPhase")) + 
  scale_fill_manual(
    values = c("#181818","#F28023","#5A870A","#009BB0","#A99CD9","#B4450E"))+
  ylab("Count")

ggsave("./output/figure_s2.png",figure_s2,
       device = ragg::agg_png,dpi = 900,
       width = 22, height = 11, units = "cm")



