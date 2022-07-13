# pulling/downloading data sheets from ACFM Feeding Trial 1


#downloading gsheet package
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(car)){install.packages("car")};library(car)
if(!require(emmeans)){install.packages("emmeans")};library(emmeans)
if(!require(multcomp)){install.packages("multcomp")};library(multcomp)
if(!require(lmerTest)){install.packages("lmerTest")};library(lmerTest)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)

#set graphic parameters
theme_set(theme_bw()+theme(panel.grid = element_blank()))

# pull the samplingcalculations.R data from ACFM Tial 1 google drive

saurl <- "https://docs.google.com/spreadsheets/d/1pPBfcCelEU_XjhyCIuJY7qCNYh-ufLbA/edit#gid=1011687095"

sturl <- "https://docs.google.com/spreadsheets/d/1pPBfcCelEU_XjhyCIuJY7qCNYh-ufLbA/edit#gid=1549175690"

samp <- read.csv(text=gsheet2text(saurl,format="csv"),stringsAsFactors = F)

stock <- read.csv(text=gsheet2text(sturl,format="csv"),stringsAsFactors = F)

#organize data a  bit
samp$diet <- factor(samp$diet,levels=c(1,2,3))
samp$diet.name <- factor(samp$diet.name,levels=c("Control (100% MFM)",
                                                 "ACFM for MFM", 
                                                 "All ACFM"))

#making linear model for HSI in samplingcalculations

HSI.lm <- lmer(HSI~diet.name+(1|tank),samp)
plot(HSI.lm)
summary(HSI.lm)
anova(HSI.lm)

# working through contrasts
(HSI.emm <- emmeans(HSI.lm, "diet.name"))
HSI.emm.df<-data.frame(HSI.emm)
contrast(HSI.emm, "tukey")

# Visualize results
ggplot(data=HSI.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("HSI")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#example of how to re-level
HSI.lm.r2 <- lmer(HSI~diet+(1|tank),samp%>%
                    mutate(diet=relevel(diet,ref="2")))
summary(HSI.lm.r2)
