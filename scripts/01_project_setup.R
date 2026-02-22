#PROJECT SET-UP

install.packages("pacman")
library(pacman)

remotes::install_github("stefanocoretta/tidygam@v1.0.0")

pacman::p_load(nhanesA, tidyverse, tidygam, sjlabelled, 
               Hmisc, gratia, remotes, ggh4x, patchwork, 
               mgcv, #tidymv, 
               calecopal, MatchIt, broom)

source("./R/project_functions.R")

