#PROJECT SET-UP

install.packages("pacman")
library(pacman)

pacman::p_load(nhanesA, tidyverse, tidygam, sjlabelled, 
               Hmisc, gratia, remotes, ggh4x, patchwork, 
               mgcv, tidymv, calecopal, MatchIt, broom)

source("./R/project_functions.R")

