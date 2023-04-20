#Trial 1 & 2 PRE and ERE Calculations
#A. Host 1/24/23

# load packages----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(readxl))install.packages("readxl");library(readxl)
if(!require(car)){install.packages("car")};library(car)
if(!require(emmeans)){install.packages("emmeans")};library(emmeans)
if(!require(multcomp)){install.packages("multcomp")};library(multcomp)
if(!require(lmerTest)){install.packages("lmerTest")};library(lmerTest)

# bring in data----
source("scripts/download_data-EX.R")

trial1_erepre <- read.csv("odata/trial1_erepre.csv")
trial2_erepre <- read.csv("odata/trial2_erepre.csv")

#establishing contrasts
conts<-list(control.all=c(1,0,-1),
            control.formfm=c(1,-1,0),
            formfm.all=c(0,1,-1))

trial1_erepre$diet.name <- factor(trial1_erepre$diet.name,levels=c("Control (100% MFM)",
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

trial2_erepre$diet.name <- factor(trial2_erepre$diet.name,levels=c("Control (100% MFM)",
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


#contrasts for PRE, trial 1
pre1.lm <- lm(pre~diet.name,trial1_erepre)
plot(pre1.lm)
summary(pre1.lm)
anova(pre1.lm)
(pre1.emm <- emmeans(pre1.lm, "diet.name"))
pre1.emm.df<-data.frame(pre1.emm)
contrast(pre1.emm, conts,adjust="sidak")

ggplot(data=pre1.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("PRE")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for ERE, trial 1
ere1.lm <- lm(ere~diet.name,trial1_erepre)
plot(ere1.lm)
summary(ere1.lm)
anova(ere1.lm)
(ere1.emm <- emmeans(ere1.lm, "diet.name"))
ere1.emm.df<-data.frame(ere1.emm)
contrast(ere1.emm, conts,adjust="sidak")

ggplot(data=ere1.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("ERE")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for PRE, trial 2
pre2.lm <- lm(pre~diet.name,trial2_erepre)
plot(pre2.lm)
summary(pre2.lm)
anova(pre2.lm)
(pre2.emm <- emmeans(pre2.lm, "diet.name"))
pre2.emm.df<-data.frame(pre2.emm)
contrast(pre2.emm, conts,adjust="sidak")

ggplot(data=pre2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("PRE")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for ERE, trial 2
ere2.lm <- lm(ere~diet.name,trial2_erepre)
plot(ere2.lm)
summary(ere2.lm)
anova(ere2.lm)
(ere2.emm <- emmeans(ere2.lm, "diet.name"))
ere2.emm.df<-data.frame(ere2.emm)
contrast(ere2.emm, conts,adjust="sidak")

ggplot(data=ere2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("ERE")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)
