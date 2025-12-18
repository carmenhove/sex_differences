
m1.plots <- m1.preds %>% 
  #mutate(Value = case_when(Measure %in% c("WBC","NEU","LYM","NLR","EOS","MON") ~ log(Value),
  #                         Measure == "BAS" ~ Value)) %>% 
                           #Measure %in% c("EOS","MON","BAS") ~ Value)) %>% 
  group_by(Repcat,Population,Measure,Age,Sex) %>% 
  summarise(Lower = min(Value,na.rm = T),
            Upper = max(Value, na.rm = T),
            Med = median(Value, na.rm = T)) %>% 
  mutate(Repcat = ordered(Repcat, levels = c("Prepubertal",
                                             "Reproductive",
                                             "PostReproductive")))


m2.plots <- m2.preds %>% 
 # mutate(Value = case_when(Measure %in% c("WBC","NEU","LYM","NLR","EOS","MON") ~ log(Value),
  #                         Measure == "BAS" ~ Value)) %>% 
  #mutate(Value = case_when(Measure %in% c("WBC","NEU","LYM","NLR") ~ log(Value),
  #                         Measure %in% c("EOS","MON","BAS") ~ Value)) %>% 
  group_by(Repcat2,Population,Measure,Age) %>% 
  summarise(Lower = min(Value,na.rm = T),
            Upper = max(Value, na.rm = T),
            Med = median(Value, na.rm = T)) %>%  
  mutate(Measure = ordered(Measure, levels = c("WBC","NEU","LYM",
                                        "EOS","MON","BAS","NLR")),
         Repcat = "Reproductive",
         Group = case_when(Repcat2 == "FemaleCycling" ~ "Cycling",
                           Repcat2 == "FemalePregnant" ~ "Pregnant",
                           Repcat2 == "FemalePostpartum" ~ "Postpartum",
                           Repcat2 == "MaleMale" ~ "Male"),
         Group = ordered(Group, levels = c("Male","Cycling",
                                           "Pregnant","Postpartum")))#,
         #Measure.type = case_when(Measure %in% c("WBC","NEU","LYM",
         #                                        "EOS","MON","BAS") ~ "cells/uL",
         #                         Measure == "NLR" ~ "Ratio"))

#age.limits <- quantile(m2.preds$Age, probs = c(0.05,0.95))

#m3.plots <- m2.preds %>% 
#  mutate(Age.cat = case_when(Age == age.limits[[1]] ~ "5% Quantile",
#                             Age == age.limits[[2]] ~ "95% Quantile")) %#>% 
#  filter(!is.na(Age.cat))

#m3.plots.medians <- m3.plots %>% 
#  group_by(Repcat2,Population,Measure,Age.cat) %>% 
#  summarise(Lower = min(Value,na.rm = T),
 #           Upper = max(Value, na.rm = T),
 #           Med = median(Value, na.rm = T))


install.packages("devtools")
library(devtools)
devtools::install_github("an-bui/calecopal")
library(calecopal)

#m1.plotlist <- split(m1.plots, list(m1.plots$Measure, m1.plots$Population))

#Figure 1s
Fig1.plots <- function(x){
  #Unit = unique(x$Measure.type)
  Population = unique(x$Population)

  Fig1 <- ggplot(x,
    aes(x = Age, y = Med, color=Sex, fill=Sex))+
    geom_line(size = 0.7)+
    geom_ribbon(aes(ymin = Lower, ymax =Upper, group = Sex),
                alpha=0.6, linetype = 0) +
    facet_grid(Population ~ Repcat, scales = "free") + 
    scale_fill_manual(values = cal_palette("tidepool"))+
    scale_color_manual(values = cal_palette("tidepool"))+
    theme(legend.position = "right")#+
    #ylab(Unit) + 
    #labs(title = Population)+
    #theme(legend.position = "bottom")
  
  #Fig1B <- ggplot(x %>% filter(Population == "THLHP"),
  #                aes(x = Age, y = Med, color=Sex, fill=Sex))+
  #  geom_line(size = 0.7)+
  #  geom_ribbon(aes(ymin = Lower, ymax =Upper, group = Sex),
  #              alpha=0.6, linetype = 0) +
  #  facet_grid(Measure ~ Repcat, scales = "free") + 
  #  scale_fill_manual(values = cal_palette("tidepool"))+
  #  scale_color_manual(values = cal_palette("tidepool"))#+
    #ylab(Unit) + 
    #labs(title = Population)
  
 # Fig1 <- ggarrange(Fig1A, Fig1B,
 #                   ncol = 1,
 #                   common.legend = T,
 #                   legend = "right")
  
  Fig1
}




Fig2.plots <- function(x){
  Population = unique(x$Population)
  
  Fig2 <- ggplot(x,
                 aes(x = Age, y = Med, color=Group, fill=Group))+
    geom_line(size = 0.7)+
    geom_ribbon(aes(ymin = Lower, ymax =Upper, group = Group),
                alpha=0.6, linetype = 0) +
    facet_grid(Population ~ Repcat, scales = "free") + 
    scale_fill_manual(values = cal_palette("superbloom3"))+
    scale_color_manual(values = cal_palette("superbloom3"))+
    theme(legend.position = "right")+
    labs(title = "")
  Fig2
}

#TOTAL LEUKOCYTES
Fig1a <- Fig1.plots(m1.plots %>% filter(Measure == "WBC"))+
  ylab("ln cells/uL") + labs(tag = "A", title = "Total Leukocytes")
Fig1b <- Fig2.plots(m2.plots %>% filter(Measure == "WBC")) +
  ylab("ln cells/uL")+labs(tag = "B")

ggarrange(Fig1a, Fig1b, ncol = 2, widths = c(1.5,1))
ggsave("Figure1.tiff", width = 10, height = 5, units="in")

#NEUTROPHILS 
Fig2a <- Fig1.plots(m1.plots %>% filter(Measure == "NEU"))+
  ylab("ln cells/uL") + labs(tag = "A", title = "Neutrophils")
Fig2b <- Fig2.plots(m2.plots %>% filter(Measure == "NEU")) +
  ylab("ln cells/uL")+labs(tag = "B")

ggarrange(Fig2a, Fig2b, ncol = 2, widths = c(1.5,1))
ggsave("Figure2.tiff", width = 10, height = 5, units="in")

#LYMPHOCYTES
Fig3a <- Fig1.plots(m1.plots %>% filter(Measure == "LYM"))+
  ylab("ln cells/uL") + labs(tag = "A", title = "Lymphocytes")
Fig3b <- Fig2.plots(m2.plots %>% filter(Measure == "LYM")) +
  ylab("ln cells/uL")+labs(tag = "B")

ggarrange(Fig3a, Fig3b,  ncol = 2, widths = c(1.5,1))
ggsave("Figure3.tiff", width = 10, height = 5, units="in")

#NLR
Fig4a <- Fig1.plots(m1.plots %>% filter(Measure == "NLR"))+
  ylab("ln ratio") + labs(tag = "A", title = "NLR")
Fig4b <- Fig2.plots(m2.plots %>% filter(Measure == "NLR")) +
  ylab("ln ratio")+labs(tag = "B")

ggarrange(Fig4a, Fig4b, ncol = 2, widths = c(1.5,1))
ggsave("Figure4.tiff", width = 10, height = 5, units="in")

#EOS
Fig5a <- Fig1.plots(m1.plots %>% filter(Measure == "EOS"))+
  ylab("ln cells/uL") + labs(tag = "A", title = "Eosinophils")
Fig5b <- Fig2.plots(m2.plots %>% filter(Measure == "EOS")) +
  ylab("ln cells/uL")+labs(tag = "B")

ggarrange(Fig5a, Fig5b, ncol = 2, widths = c(1.5,1))
ggsave("Figure5.tiff", width = 10, height = 5, units="in")

#MONOCYTES
Fig6a <- Fig1.plots(m1.plots %>% filter(Measure == "MON"))+
  ylab("ln cells/uL") + labs(tag = "A", title = "Monocytes")
Fig6b <- Fig2.plots(m2.plots %>% filter(Measure == "MON")) +
  ylab("ln cells/uL")+labs(tag = "B")

ggarrange(Fig6a, Fig6b, ncol = 2, widths = c(1.5,1))
ggsave("Figure6.tiff", width = 10, height = 5, units="in")

#BASOPHILS
Fig7a <- Fig1.plots(m1.plots %>% filter(Measure == "BAS"))+
  ylab("cells/uL") + labs(tag = "A", title = "Basophils")
Fig7b <- Fig2.plots(m2.plots %>% filter(Measure == "BAS")) +
  ylab("cells/uL")+labs(tag = "B")

ggarrange(Fig7a, Fig7b, ncol = 2, widths = c(1.5,1))
ggsave("Figure7.tiff", width = 10, height = 5, units="in")

ggplot(matched.df, 
       aes(x = Age, y = NumPartos, color = Population)) + 
  geom_point(position = "jitter") + 
 facet_grid(~ Population, scales = "free")



