#ACFM_Trial2
#Pulling data, calculations, & figures for sampling data and stockout data

#downloading gsheet package
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(car)){install.packages("car")};library(car)
if(!require(emmeans)){install.packages("emmeans")};library(emmeans)
if(!require(multcomp)){install.packages("multcomp")};library(multcomp)
if(!require(lmerTest)){install.packages("lmerTest")};library(lmerTest)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)

#pull the samplingcalculations.2.R and stockoutcaculations.2.R data from ACFM Trial 1 google drive

sa2url <- "https://docs.google.com/spreadsheets/d/1gJiMiNsxcvK8JgB851b56es41gPggkMqzdFpDmT4WDc/edit?pli=1#gid=815912700"
st2url

samp2 <- read.csv(text=gsheet2text(sa2url,format="csv"),stringsAsFactors = F)
stock2



#SAMPLING DATA TRIAL 2 CALCULATIONS
#organize data
samp2$diet <- factor(samp2$diet,levels=c(4,5,6))
samp2$diet.name <- factor(samp2$diet.name,levels=c("Control (MFO 100)",
                                                 "ACFO 50 / MFO 50", 
                                                 "ACFO 100"))

#making linear model for HSI in samplingcalculations.2.r

HSI.2.lm <- lmer(HSI~diet.name+(1|tank),samp2)
plot(HSI.2.lm)
summary(HSI.2.lm)
anova(HSI.2.lm)

# working through contrasts
(HSI.2.emm <- emmeans(HSI.2.lm, "diet.name"))
HSI.2.emm.df<-data.frame(HSI.2.emm)
contrast(HSI.2.emm, conts,adjust="sidak")

# Visualize results
ggplot(data=HSI.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("HSI")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)
