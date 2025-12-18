
#set working directory
setwd("/Users/carmenhove/Desktop/GRAD SCHOOL/PROJECTS/Sex Differences dissertation") 

# package names
pkgs = c("nhanesA","ggpubr","FSA","stringi","csv","gdata","tidyverse","mgcv","tidymv","foreach","reshape2","extrafont","readr","Hmisc","RColorBrewer","scales","cowplot","lavaan","extrafont","MatchIt","mgcViz","voxel")

#install packages
install.packages(pkgs)

#load package libraries
inst = lapply(pkgs, library, character.only = TRUE) 

#install.packages("rstan", type = "source")
#install rstan from source
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

library(rstan)

load ('SexDiffEnvironment.RData')

#split violin function
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


#Raincloud plots
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
          },
  
  draw_group = function(data, panel_scales, coord) {
    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x,
                      xmaxv = x + violinwidth * (xmax - x))
    
    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                     plyr::arrange(transform(data, x = xmaxv), -y))
    
    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    
    ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },
  
  draw_key = draw_key_polygon,
  
  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                    alpha = NA, linetype = "solid"),
  
  required_aes = c("x", "y")
  )

##---------------------------------------------------------------------------
#COMPILE NHANES DATASET
#load all files containing demographic information (collection years 2003-2015)
Demographics<-bind_rows(One=nhanes('DEMO_C'), Two=nhanes('DEMO_D'), 
                        Three=nhanes('DEMO_E'),Four=nhanes('DEMO_F'), 
                        Five=nhanes('DEMO_G'),Six=nhanes('DEMO_H'), 
                        Seven=nhanes('DEMO_I'),.id="Year")

#load sex hormone information (collection years 2013-2015)
Hormones<-bind_rows(Six=nhanes('TST_H'),Seven=nhanes('TST_I'),.id="Year")

# load HIV information (collection years 2005-2007)
HIV<-bind_rows(One=nhanes('L03_C'), Two=nhanes('HIV_D'),.id="Year")

# load files containing BMI data (collection years 2003-2015)
BMI<-bind_rows(One=nhanes('BMX_C'), Two=nhanes('BMX_D'),
               Three=nhanes('BMX_E'),Four=nhanes('BMX_F'),
               Five=nhanes('BMX_G'),Six=nhanes('BMX_H'),
               Seven=nhanes('BMX_I'),.id="Year")

SMK<-bind_rows(One = nhanes('SMQ_C'), Two = nhanes('SMQ_D'),
               Three = nhanes('SMQ_E'), Four = nhanes('SMQ_F'),
               Five = nhanes('SMQ_G'), Six = nhanes('SMQ_H'),
               Seven = nhanes('SMQ_I'),.id="Year")

# Load files containing reproductive health questionnaire data (collection years 2003-2015)
RH_C<-nhanes('RHQ_C') #2003-2004 *has depreciated variables (RHQ031, RHQ170)
RH_D<-nhanes('RHQ_D') #2005-2006 *has depreciated variable (RHQ031)

#Fix depreciated variables
RH_C<-plyr::rename(RH_C, c("RHQ031"="RHQ_NA", "RHD170"="RHD171")) 
RH_D<-plyr::rename(RH_D, c("RHQ031"="RHQ_NA"))

RepHealth<-bind_rows(One=RH_C,Two=RH_D,Three=nhanes('RHQ_E'),
                     Four=nhanes('RHQ_F'),Five=-nhanes('RHQ_G'),
                     Six=nhanes('RHQ_H'),Seven=nhanes('RHQ_I'), .id="year")

# C-reactive protein (mg/dL) (collection years 2003-2009)
CRP<-bind_rows(One=nhanes('L11_C'), Two=nhanes('CRP_D'), 
               Three=nhanes('CRP_E'), Four=nhanes('CRP_F'), .id="Year")

# Complete blood counts (1000 cells/uL) (collection years 2003-2015)
CBC<- bind_rows(One=nhanes('L25_C'), Two=nhanes('CBC_D'), 
                Three=nhanes('CBC_E'), Four=nhanes('CBC_F'),
                Five=nhanes('CBC_G'), Six=nhanes('CBC_H'),
                Seven=nhanes('CBC_I'),.id="Year")

#Serum total IgE (kU/L) (collection year 2005)
###Allergy-specific antibodies
## LBDF13LC (peanut), LBDF24LC (shrimp), LBDID1LC / LBDID2LC (dust mites),
## LBDIE1LC (Cat), LBDIE5LC (dog), LBDIF1LC (Egg), LBDIF2LC (Milk), LBDIG2LC (grass) in AL_IGE_*
IgE<-nhanes('AL_IGE_D')
IgE$Year<-c(rep("Two",length(IgE$SEQN)))

Health.history <- bind_rows(One = nhanes('MCQ_C'), Two = nhanes('MCQ_D'),
                     Three = nhanes('MCQ_E'), Four = nhanes('MCQ_F'),
                     Five = nhanes('MCQ_G'), Six = nhanes('MCQ_H'), 
                     Seven = nhanes('MCQ_I'),
                     .id = "Year")

Health.history$MCQ190
#arthritis type
table(Health.history$MCQ190)

#Cytomegalovirus LBXIGG, LBXIGGA, LBXIGM Recode variables??
Cyto.virus <- bind_rows(One = nhanes('SSCMV_C'), Five = nhanes('CMV_G'), 
                        Eight = nhanes('CMV_J'), .id = "Year")             

#Hepatitis C antigen - confirmed (LBDHCV in all kinds of datatable names)
#Bacterial Vaginosis? BLXBV in I34_b, I34_c, 
#Trichomonas vaginalis? LBXTV in I34_bc, 
#Heptatitis-specific antibodies

#Herpes Simplex Virus Type-1 and Type-2 
##1999-2000, 2001-2002 = LBXHE1 and LBXHE2 in lab09, 109_b, 109_c
Herpes <- bind_rows(One = nhanes('HSV_C'), Two = nhanes('HSV_D'),
                    Three = nhanes('HSV_E'), Four = nhanes('HSV_F'),
                    Five = nhanes('HSV_G'), Six = nhanes('HSV_H'),
                    Seven = nhanes('HSV_I'), .id = "Year")             

HPV.vaginal <- bind_rows(One = nhanes('L37SWA_C'), One = nhanes('HPVSWR_D'),
                         Two = nhanes('HPVSWR_D'),Three = nhanes('HPVSWR_E'), 
                         Four = nhanes('HPVSWR_F'),
                         Five = nhanes('HPVSWR_G'), Six = nhanes('HPVSWR_H'),
                         Seven = nhanes('HPVSWR_I'), .id = "Year")             

HPV.penile <- bind_rows(Six = nhanes('HPVP_H'),Seven = nhanes('HPVP_I'), 
                        .id = "Year")   

Chlamydia <- bind_rows(One = nhanes('MCQ_C'), Two = nhanes('MCQ_D'),
                       Three = nhanes('MCQ_E'), Four = nhanes('MCQ_F'),
                       Five = nhanes('MCQ_G'), Six = nhanes('MCQ_H'), 
                       Seven = nhanes('CHLMDA_I'),
                       .id = "Year")

NHdata.1 <- full_join(Demographics, BMI) %>% 
  full_join(., CBC) %>%
  full_join(., CRP) %>%
  full_join(., IgE) %>%
  full_join(., RepHealth) %>%
  full_join(., Hormones) %>%
  full_join(., Health.history) %>%
  full_join(., HIV) %>%
  full_join(., Cyto.virus) %>%
  full_join(., Herpes) %>%
  full_join(., HPV.vaginal) %>%
  full_join(., HPV.penile) %>%
  full_join(., Chlamydia) %>%
  full_join(., SMK)


NHdata.1$BMXBMI

#NOTES for revisions
#1. Outliers in Estrogen (clinical range = 15 - 350 pg/mL) among women
#2. Outliers in Testosterone (clinical range = 15-70 ng/dL) among women
#3. Estrogen is in pg/mL, testosterone is in ng/dL

#change NHANES variable names to match THLHP, transform 1000 cells/ul to cells/uL AND md/dL to mg/L
NHdata.2<-NHdata.1 %>%
  mutate(WBC = LBXWBCSI*1000, NEU = LBDNENO*1000, LYM = LBDLYMNO*1000, 
         MON = LBDMONO*1000, BAS = LBDBANO*1000, EOS = LBDEONO*1000, 
         IgE = as.numeric(LBXIGE), crp = LBXCRP*10, pid = as.character(SEQN), 
         BMI = BMXBMI, Age = RIDAGEYR, NumPreg = abs(RHQ160), NumPartos=abs(RHQ171), 
         Estrogen = LBXEST, Testosterone = LBXTST, EstTest = Estrogen/Testosterone,
         TestEst= Testosterone/Estrogen, HIV =LBDHI, CD4Count = LBXCD4, 
         CD8Count = LBXCD8, CD4CD8 = LBXCD4/LBXCD8, Asthma.current = MCQ035,
         Arthritis.current = MCQ160A, Arthritis.type = MCQ191, Arthritis.age = MCQ180A,
         Endometriosis = RHQ360,
         Celiac = MCQ082, Psoriasis = MCQ070, Cancer = MCQ220, Cancer.type1 = MCQ230A,
         Cancer.type2 = MCQ230B, Cancer.type3 = MCQ230C, Cancer.type4 = MCQ230D,
         Testicular.age = MCQ240AA, Uterine.age = MCQ240CC, Breast.age = MCQ240E,
         Cervical.age = MCQ240F, Ovarian.age = MCQ240S) 

# create variable for sex with two levels matching THLHP *Original NHANES values are opposite*
# Change to 0 = male, 1 = female (original: 1 = male, 2 = female)
NHdata.2$Sex<-NULL
NHdata.2$Sex[NHdata.2$RIAGENDR==1]<-0
NHdata.2$Sex[NHdata.2$RIAGENDR==2]<-1
NHdata.2$Sex<-as.factor(NHdata.2$Sex)
table(NHdata.2$Sex)


#COMPILE TSIMANE DATASET
TSdata.1<-read.csv("BiochemFlo_copy.csv", stringsAsFactors = FALSE)

#remove old "sex" variable, depreciated for this dataset
TSdata.1$Sex<-NULL 
#create levels for sex vector, mutated to match coding values in NHANES dataset
#Change to 0 = male, 1 = female
TSdata.1$Sex[TSdata.1$male==1 | TSdata.1$Trimester %in% c("Male","Juvenile Male")]<-0
TSdata.1$Sex[TSdata.1$male==0 & TSdata.1$Trimester %in% 
               c("Lactating","Pre-Menst","Cycling","1","2","3","Menopause")]<-1
TSdata.1$Sex<-factor(TSdata.1$Sex)

TSdata.2 <- TSdata.1 %>%
  mutate(NEU = NeutroCnt*1000, LYM=(WBC*Lymphocitos/100)*1000, 
         MON=MonoCnt*1000, BAS=BasoCnt*1000,EOS=EosinCnt*1000, 
         CD4CD8 = CD48Ratio, Age = abs(Age), 
         RepPhase = Trimester, MonthPP=as.integer(round(IPI, digits=0))) %>% 
  mutate(WBC = WBC*1000) 
table(TSdata.2$Sex)

##--------------------------------------------------------------------------------------
## DATASET 1: immune measures ~ Reproductive Group (Male > nuliparous cycling > parous cycling > pregnant > postpartum)

#add "GROUP" to NHANES data
NH.D1<-NHdata.2 %>%
  mutate(Group = NA,
         Population = "NHANES")

## Male
NH.D1$Group[NH.D1$Sex==0]<-"Male"
##regularily cycling + have never been pregnant
NH.D1$Group[NH.D1$RHQ031 == 1 & NH.D1$RHQ131==2]<-"Nulliparous cycling"
## regularily cycling + have been pregnant
NH.D1$Group[NH.D1$RHQ031 == 1 & NH.D1$RHQ131==1]<-"Parous cycling"
## currently pregnant
NH.D1$Group[NH.D1$RHD143 == 1 | NH.D1$RIDEXPRG==1]<-"Pregnant"
## <= 6 months postpartum
NH.D1$Group[NH.D1$RHQ197 <= 6]<-"Postpartum"


#add "GROUP" to THLHP
TS.D1<-TSdata.2 %>%
  mutate(Group = NA,
         Population = "THLHP")
## male
TS.D1$Group[TS.D1$Sex==0]<-"Male"
TS.D1$Group[TS.D1$Trimester == "Cycling" & TS.D1$NumGestas==0]<-"Nulliparous cycling"
TS.D1$Group[TS.D1$Trimester == "Cycling" & TS.D1$NumGestas>=1]<-"Parous cycling"
TS.D1$Group[TS.D1$Trimester %in% c("1","2","3")]<-"Pregnant"
TS.D1$Group[TS.D1$MonthPP <= 6]<-"Postpartum"

D1<-full_join(NH.D1, TS.D1) %>%
  filter(Age > 18 & Age <45)

#Graph population differences in each reproductive group
ggplot(D1[!is.na(D1$Group),], aes(y = log(WBC), x = Group, fill = Population)) +
  geom_flat_violin(position = position_nudge(x = 0, y = 0), alpha = .8) +
  coord_flip()


#Endometriosis
ggplot(NH.D1 %>%
         filter(Age > 18, Age < 55) %>%
          filter(Endometriosis %in% c(1,2)) %>%
         mutate(Endometriosis = as.factor(Endometriosis)),
aes(y = log(LYM), x = Sex, fill = Endometriosis)) +
  geom_split_violin()



ggplot(NH.D1,
       aes(y = log(LYM), x = Asthma.current)) +
  geom_split_violin()





#NOTES for revisions
#1. Outliers in Estrogen (clinical range = 15 - 350 pg/mL) among women
#2. Outliers in Testosterone (clinical range = 15-70 ng/dL) among women
#3. Estrogen is in pg/mL, testosterone is in ng/dL

#ESTROGEN
ggplot(NH.D1 %>%
         filter(Age>18,
                !is.na(Group)),
       aes(y = log(Estrogen+1), x = Group, fill = Sex)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) + 
  coord_flip() + 
  ylab("Estradiol (ln pg/mL)") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values=c("chartreuse4","orange2")) 

#TESTOSTERONE
ggplot(NH.D1 %>%
         filter(Age>18,
                !is.na(Group)),
       aes(y = log(Testosterone+1), x = Group, fill = Sex)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) + 
  coord_flip() + 
  ylab("Testosterone (ln ng/mL)") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values=c("chartreuse4","orange2")) 

#EST:TEST
ggplot(NH.D1 %>%
         filter(Age>18,
                !is.na(Group)),
       aes(y = log(EstTest), x = Group, fill = Sex)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) + 
  coord_flip() + 
  ylab("Estrogen:Testosterone Ratio") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values=c("chartreuse4","orange2")) 

#overall difference
ggplot(NH.D1 %>%
         filter(Age>18,
                !is.na(Group)),
       aes(x = log(Estrogen+1), fill = Sex)) +
  geom_density(alpha = .8) +
  xlab("Estradiol (ln pg/mL)") +
  scale_fill_discrete(name = "Sex", 
                      labels = c("Male", "Female")) + 
  theme(axis.title.y = element_blank())

#TESTOSTERONE BY AGE
ggplot(NH.D1 %>%
         filter(!is.na(Sex)), 
       aes(x = Age, y = log(Testosterone+1), color = Sex)) + 
  geom_point(size=2, position="jitter") +
  ylab("Testosterone (ln ng/mL)") +
  scale_color_manual(labels = c("Male", "Female"), values = c("chartreuse4","orange2")) 

#ESTROGEN BY AGE
ggplot(NH.D1 %>%
         filter(!is.na(Sex)), 
       aes(x = Age, y = log(Estrogen+1), color = Sex)) + 
  geom_point(size=2, position="jitter") +
  ylab("Estradiol (ln pg/mL)") +
  scale_color_manual(labels = c("Male", "Female"), values = c("chartreuse4","orange2")) 

#EST: TEST BY AGE
ggplot(NH.D1 %>%
         filter(!is.na(Sex)), 
       aes(x = Age, y = log(EstTest), color = Sex)) + 
  geom_point(size=2, position="jitter") +
  ylab("Estrogen:Testosterone ratio")  +
  scale_color_manual(labels = c("Male", "Female"), values = c("chartreuse4","orange2")) 


ggplot(NH.D1 %>%
         filter(Age>60),
       aes(x = log(EstTest), fill = Sex)) +
  geom_density(alpha = .8) +
  xlab("Estradiol (ln pg/mL)") +
  scale_fill_discrete(name = "Sex", 
                      labels = c("Male", "Female")) + 
  theme(axis.title.y = element_blank())

#TESTOSTERONE
ggplot(NH.D1 %>%
         filter(Age>18,
                !is.na(Group)),
       aes(y = log(Testosterone+1), x = Group, fill = Sex)) + 
  geom_flat_violin(position = position_nudge(x = 0, y = 0), alpha = .8) +
  coord_flip() + ylab("Testosterone (ln ng/mL)")+
    scale_fill_discrete(name = "Sex", labels = c("Male", "Female"))+ 
  theme(axis.title.y = element_blank())
  
#overall difference
ggplot(NH.D1 %>%
         filter(Age>18),
       aes(x = log(Testosterone+1), fill = Sex)) +
  geom_density(alpha = .8)+
  xlab("Testosterone (ln ng/mL)") +
  scale_fill_discrete(name = "Sex", 
                      labels = c("Male", "Female")) + 
  theme(axis.title.y = element_blank())





#Graph sex differences for each population
ggplot(D1[!is.na(D1$Group),], aes(y = log(WBC), x = Group, fill = Group)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log(WBC), color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  facet_grid(~ Population)+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme

NH.D1
