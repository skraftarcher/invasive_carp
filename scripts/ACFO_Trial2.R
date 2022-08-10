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

#making linear model for VSI in samplingcalculations.2.r

VSI.2.lm <- lmer(VSI~diet.name+(1|tank),samp2)
plot(VSI.2.lm)
summary(VSI.2.lm)
anova(VSI.2.lm)
#contrasts for VSI
(VSI.2.emm <- emmeans(VSI.2.lm, "diet.name"))
VSI.2.emm.df <- data.frame(VSI.2.emm)
contrast(VSI.2.emm, conts,adjust="sidak")

#visualizing VSI contrast results
ggplot(data=VSI.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("VSI")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for MR in samplingcalculations.2.r

MR.2.lm <- lmer(MR~diet.name+(1|tank),samp2)
plot(MR.2.lm)
summary(MR.2.lm)
anova(MR.2.lm)
#contrasts for MR
(MR.2.emm <- emmeans(MR.2.lm, "diet.name"))
MR.2.emm.df <- data.frame(MR.2.emm)
contrast(MR.2.emm, conts,adjust="sidak")

#visualizing MR contrast results
ggplot(data=MR.2.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+geom_point()+
  xlab("")+
  ylab("Muscle Ratio")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))


#making linear model for cond.factor in samplingcalculations.2.r

cf.2.lm <-lmer(cond.factor~diet.name+(1|tank),samp2)
plot(cf.2.lm)
summary(cf.2.lm)
anova(cf.2.lm)
#contrasts for condition factor
(cf.2.emm<-emmeans(cf.2.lm,"diet.name"))
cf.2.emm.df<-data.frame(cf.2.emm)
contrast(cf.2.emm, conts,adjust="sidak")

#visualizing condition factor contrast results
ggplot(data=cf.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)+
  geom_point()+
  xlab("")+
  ylab("Condition Factor")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))
