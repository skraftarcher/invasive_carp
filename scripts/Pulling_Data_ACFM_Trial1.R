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

#pull the samplingcalculations.R and stockoutcaculations.R data from ACFM Trial 1 google drive

saurl <- "https://docs.google.com/spreadsheets/d/1pPBfcCelEU_XjhyCIuJY7qCNYh-ufLbA/edit#gid=1011687095"

sturl <- "https://docs.google.com/spreadsheets/d/1pPBfcCelEU_XjhyCIuJY7qCNYh-ufLbA/edit#gid=1549175690"

samp <- read.csv(text=gsheet2text(saurl,format="csv"),stringsAsFactors = F)

stock <- read.csv(text=gsheet2text(sturl,format="csv"),stringsAsFactors = F)



#SAMPLING DATA CALCULATIONS
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


#making linear model for VSI in samplingcalculations

VSI.lm <- lmer(VSI~diet.name+(1|tank),samp)
plot(VSI.lm)
summary(VSI.lm)
anova(VSI.lm)
#contrasts for VSI
(VSI.emm <- emmeans(VSI.lm, "diet.name"))
VSI.emm.df <- data.frame(VSI.emm)
contrast(VSI.emm, "tukey")
#visualizing VSI contrast results
ggplot(data=VSI.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("VSI")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for MR in samplingcalculations

MR.lm <- lmer(MR~diet.name+(1|tank),samp)
plot(MR.lm)
summary(MR.lm)
anova(MR.lm)
#contrasts for MR
(MR.emm <- emmeans(MR.lm, "diet.name"))
MR.emm.df <- data.frame(MR.emm)
contrast(MR.emm, "tukey")
#visualizing MR contrast results
ggplot(data=MR.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("MR")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for cond.factor in sampling calculations

cf.lm <-lmer(cond.factor~diet.name+(1|tank),samp)
plot(cf.lm)
summary(cf.lm)
anova(cf.lm)
#contrasts for condition factor
(cf.emm<-emmeans(cf.lm,"diet.name"))
cf.emm.df<-data.frame(cf.emm)
contrast(cf.emm,"tukey")
#visualizing condition factor contrast results
ggplot(data=cf.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Condition Factor")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)




#STOCKOUT DATA CALCULATIONS
#reorganize data
stock$diet <- factor(stock$diet,levels=c(1,2,3))
stock$diet.name <- factor(stock$diet.name,levels=c("Control (100% MFM)",
                                                 "ACFM for MFM", 
                                                 "All ACFM"))
#going to use emmeans for contrasts b/c it is the suggested new version of lsmeans, which is the contrast package used on the R walkthrough

#making linear model for total.g.gain in sampling calculations

total.g.gain.lm <-lm(total.g.gain~diet.name,stock)
plot(cf.lm)
summary(cf.lm)
anova(cf.lm)
# working through contrasts
(total.g.gain.emm <- emmeans(total.g.gain.lm, "diet.name"))
total.g.gain.emm.df<-data.frame(total.g.gain.emm)
contrast(total.g.gain.emm, "tukey")
#visualizing condition factor contrast results
ggplot(data=total.g.gain.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Total grams gained per Fish")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for avgFCR.all in sampling calculations

avgFCR.all.lm <-lm(avgFCR.all~diet.name,stock)
plot(avgFCR.all.lm)
summary(avgFCR.all.lm)
anova(avgFCR.all.lm)
# working through contrasts
(avgFCR.all.emm <- emmeans(avgFCR.all.lm, "diet.name"))
avgFCR.all.emm.df<-data.frame(avgFCR.all.emm)
contrast(avgFCR.all.emm, "tukey")
#visualizing condition factor contrast results
ggplot(data=avgFCR.all.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Average Feed Conversion Ratio for 8 Week Trial")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for avgFI.all in sampling calculations

avgFI.all.lm <-lm(avgFI.all~diet.name,stock)
plot(avgFI.all.lm)
summary(avgFI.all.lm)
anova(avgFI.all.lm)
# working through contrasts
(avgFI.all.emm <- emmeans(avgFI.all.lm, "diet.name"))
avgFI.all.emm.df<-data.frame(avgFI.all.emm)
contrast(avgFI.all.emm, "tukey")
#visualizing condition factor contrast results
ggplot(data=avgFI.all.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Total Average Feed Intake for 8 Week Trial")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for FCR.all in sampling calculations

FCR.all.lm <-lm(FCR.all~diet.name,stock)
plot(FCR.all.lm)
summary(FCR.all.lm)
anova(FCR.all.lm)
# working through contrasts
(FCR.all.emm <- emmeans(FCR.all.lm, "diet.name"))
FCR.all.emm.df<-data.frame(FCR.all.emm)
contrast(FCR.all.emm, "tukey")
#visualizing condition factor contrast results
ggplot(data=FCR.all.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Total Feed Conversion Ratio for 8 Week Trial")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for pincrease.all in sampling calculations

pincrease.all.lm <-lm(pincrease.all~diet.name,stock)
plot(pincrease.all.lm)
summary(FCR.all.lm)
anova(pincrease.all.lm)
# working through contrasts
(pincrease.all.emm <- emmeans(pincrease.all.lm, "diet.name"))
pincrease.all.emm.df<-data.frame(pincrease.all.emm)
contrast(pincrease.all.emm, "tukey")
#visualizing condition factor contrast results
ggplot(data=pincrease.all.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Total % Biomass Increase (g) for 8 Week Trial")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)
