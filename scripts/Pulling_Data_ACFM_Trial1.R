# pulling/downloading data sheets from ACFM Feeding Trial 1


#downloading gsheet package
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(car)){install.packages("car")};library(car)
if(!require(lsmeans)){install.packages("lsmeans")};library(lsmeans)
if(!require(multcomp)){install.packages("multcomp")};library(multcomp)
if(!require(lmerTest)){install.packages("lmerTest")};library(lmerTest)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)
# pull the samplingcalculations.R data from ACFM Tial 1 google drive

saurl <- "https://docs.google.com/spreadsheets/d/1pPBfcCelEU_XjhyCIuJY7qCNYh-ufLbA/edit#gid=1011687095"

sturl <- "https://docs.google.com/spreadsheets/d/1pPBfcCelEU_XjhyCIuJY7qCNYh-ufLbA/edit#gid=1549175690"

samp <- read.csv(text=gsheet2text(saurl,format="csv"),stringsAsFactors = F)

stock <- read.csv(text=gsheet2text(sturl,format="csv"),stringsAsFactors = F)

#organize data a  bit
samp$diet <- factor(samp$diet,levels=c(1,2,3))

#making linear model for HSI in samplingcalculations

HSI.lm <- lmer(HSI~diet+(1|tank),samp)
plot(HSI.lm)
summary(HSI.lm)
#example of how to re-level
HSI.lm.r2 <- lmer(HSI~diet+(1|tank),samp%>%
                    mutate(diet=relevel(diet,ref="2")))
summary(HSI.lm.r2)
