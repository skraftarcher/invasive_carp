#Trial 1 Proximate Calculations
#A. Host 1/20/23

# load packages----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(readxl))install.packages("readxl");library(readxl)
if(!require(car)){install.packages("car")};library(car)
if(!require(emmeans)){install.packages("emmeans")};library(emmeans)
if(!require(multcomp)){install.packages("multcomp")};library(multcomp)
if(!require(lmerTest)){install.packages("lmerTest")};library(lmerTest)

#load data from proximates_dataorg_analysis script
source("scripts/proximates_dataorg_analysis.R")

#establishing contrasts
conts<-list(control.all=c(1,0,-1),
            control.formfm=c(1,-1,0),
            formfm.all=c(0,1,-1))

trials$diet.name <- factor(trials$diet.name,levels=c("Control (100% MFM)",
                                                 "ACFM for MFM", 
                                                 "All ACFM",
                                                 "Control (MFO 100)",
                                                 "ACFO 50 / MFO 50",
                                                 "ACFO 100"),
                         labels = c("Control (100% MFM)",
                                    "ICFM for MFM", 
                                    "All ICFM",
                                    "Control (MFO 100)",
                                    "ICFO 50 / ICFO 50",
                                    "ICFO 100"))
colrs<-c("#70AD47","#4A91D0","#4A91D0")
ocolrs<-c("#FFC000","#FFC000","#4A91D0")

#contrasts for proximate data
protein.ww.lm <- lm(protein.ww~diet.name,trials)
plot(protein.ww.lm)
summary(protein.ww.lm)
anova(protein.ww.lm)

#need to figure out how to split up trial 1 and trial 2 data
