
#Age limits



m1.preds

Figure3.plots <- function(x){
  
  Fig3 <- ggplot(x,aes(x = Value, color=Repcat2, fill=Repcat2))+
    geom_density(alpha = 0.5, adjust = 2)+
    scale_fill_manual(values = cal_palette("superbloom3"))+
    scale_color_manual(values = cal_palette("superbloom3"))+
    facet_grid(Age.cat ~ Measure, scales = "free")
  Fig3
  
}

#Figure 1s
Fig1.plots <- function(x){
  #Unit = unique(x$Measure.type)
  x <- x %>% 
    mutate(Repcat = ordered(
      Repcat, levels = c("Prepubertal","Reproductive",
                         "PostReproductive")))
  
  Population = unique(x$Population)
  Measure = unique(x$Measure)
  
  Fig1A <- ggplot(x %>% filter(Population == "NHANES"),
                  aes(x = Value, color=Sex, fill=Sex))+
    geom_density(alpha = 0.5, adjust = 2)+
    scale_fill_manual(values = cal_palette("superbloom3"))+
    scale_color_manual(values = cal_palette("superbloom3"))+
    facet_grid(Population ~ Repcat, scales = "free")+
    xlab("cells/uL")+
    labs(title = "Neutrophils")
    
  Fig1B <- ggplot(x %>% filter(Population == "THLHP"),
                  aes(x = Value, color=Sex, fill=Sex))+
    geom_density(alpha = 0.5, adjust = 2)+
    scale_fill_manual(values = cal_palette("superbloom3"))+
    scale_color_manual(values = cal_palette("superbloom3"))+
    facet_grid(Population ~ Repcat, scales = "free")+
    xlab("cells/uL")
  
  Fig1 <- ggarrange(Fig1A,Fig1B, 
                    ncol=1, heights = c(1,0.9),
                    common.legend = T, legend = "right")
  Fig1
}

Fig1.plots(m1.preds %>% filter(Measure == "NEU"))#+
  #labs(title = "Neutrophils") + 
  #lab("cells/uL")











#Figure 2s
#Figure 1s
Fig2.plots <- function(x){
  #Unit = unique(x$Measure.type)
  Population = unique(x$Population)
  
  Fig2 <- ggplot(x,aes(x = Age, y = Med, color=Repcat2, fill=Repcat2))+
    geom_line(size = 0.7)+
    geom_ribbon(aes(ymin = Lower, ymax =Upper, group = Repcat2),
                alpha=0.6, linetype = 0) +
    scale_fill_manual(values = cal_palette("superbloom3"))+
    scale_color_manual(values = cal_palette("superbloom3"))+
    facet_grid(Measure ~ Population, scales = "free")+
    theme(legend.position = "right")#+
  #labs(title = Population)
  Fig2
}

Fig2.plots(m2.plots)

#Fig2A = Fig2.plots(m2.plots %>% filter(Population == "NHANES")) 
#Fig2B = Fig2.plots(m2.plots %>% filter(Population == "THLHP"))
#Figure2 <- ggarrange(Fig2A,Fig2B,
#                     ncol = 2, common.legend = TRUE, 
#                     legend = "bottom")
ggsave("Figure2.tiff", width = 6, height = 8, units="in")


m3.plotlist <- split(m3.plots, list(m3.plots$Measure))
#Figure 3 plots
Figure3.plots <- function(x){
  
  Fig3 <- ggplot(x,aes(x = Value, color=Repcat2, fill=Repcat2))+
    geom_density(alpha = 0.5, adjust = 2)+
    scale_fill_manual(values = cal_palette("superbloom3"))+
    scale_color_manual(values = cal_palette("superbloom3"))+
    facet_grid(Age.cat ~ Measure, scales = "free")
  Fig3
  
}

Fig3A = Figure3.plots(m3.plotlist[["NLR"]])

