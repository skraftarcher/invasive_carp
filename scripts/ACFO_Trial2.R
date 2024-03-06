#ACFM_Trial2
#Pulling data, calculations, & figures for sampling data and stockout data

#downloading gsheet package
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(car)){install.packages("car")};library(car)
if(!require(emmeans)){install.packages("emmeans")};library(emmeans)
if(!require(multcomp)){install.packages("multcomp")};library(multcomp)
if(!require(lmerTest)){install.packages("lmerTest")};library(lmerTest)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)

#set graphic parameters
theme_set(theme_bw()+theme(panel.grid = element_blank()))
colrs<-c("#70AD47","#4A91D0","#4A91D0")
ocolrs<-c("#70AD47","#70AD47","#4A91D0")

#pull the samplingcalculations.2.R and stockoutcaculations.2.R data from ACFM Trial 1 google drive
source("scripts/download_data-EX.R")
samp2 <- read.csv("odata/trial2_samplingcalcs.csv")
stock2 <- read.csv("odata/trial2_stockingcalcs.csv")
# list of contrasts
conts2<-list(MFO.allACFO=c(1,0,-1),
            MFO.5050=c(1,-1,0),
            m5050.allACFO=c(0,1,-1))

#SAMPLING DATA TRIAL 2 CALCULATIONS
#organize data
samp2$diet <- factor(samp2$diet,levels=c(4,5,6))
samp2$diet.name <- factor(samp2$diet.name,
                          levels=c("Control (MFO 100)",
                                                 "ACFO 50 / MFO 50", 
                                                 "ACFO 100"),
                          labels=c("Control (MFO 100)",
                                   "ICFO 50 / MFO 50", 
                                   "ICFO 100"))

#making linear model for HSI in samplingcalculations.2.r

HSI.2.lm <- lmer(HSI~diet.name+(1|tank),samp2)
plot(HSI.2.lm)
summary(HSI.2.lm)
anova(HSI.2.lm)

# working through contrasts
(HSI.2.emm <- emmeans(HSI.2.lm, "diet.name"))
HSI.2.emm.df<-data.frame(HSI.2.emm)
contrast(HSI.2.emm, conts2,adjust="sidak")

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
contrast(VSI.2.emm, conts2,adjust="sidak")

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
contrast(MR.2.emm, conts2,adjust="sidak")

ggplot(data=MR.2.emm.df,
       aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),
                width=.1,color="black")+
  geom_point(shape=21,size=10,stroke=4)+
  xlab("")+
  ylab("Muscle Ratio")+
  theme(legend.position = "none",
        axis.text = element_text(size=24, color="black"),
        axis.title.y = element_text(size=24))+
  scale_fill_manual(values=colrs)+
  scale_color_manual(values=ocolrs)

ggsave("figures/trial2_MR.png",width=11,height=5)

#making linear model for cond.factor in samplingcalculations.2.r

cf.2.lm <-lmer(cond.factor~diet.name+(1|tank),samp2)
plot(cf.2.lm)
summary(cf.2.lm)
anova(cf.2.lm)
#contrasts for condition factor
(cf.2.emm<-emmeans(cf.2.lm,"diet.name"))
cf.2.emm.df<-data.frame(cf.2.emm)
contrast(cf.2.emm, conts2,adjust="sidak")

ggplot(data=cf.2.emm.df,
       aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),
                width=.1,color="black")+
  geom_point(shape=21,size=7,stroke=4)+
  xlab("")+
  ylab("Condition Factor")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))+
  scale_fill_manual(values=colrs)+
  scale_color_manual(values=ocolrs)

ggsave("figures/trial2_CF.png",width=5.99,height=4.25)


#STOCKOUT DATA TRIAL 2 CALCULATIONS
#reorganize data
stock2$diet <- factor(stock2$diet,levels=c(4,5,6))
stock2$diet.name <- factor(stock2$diet.name,levels=c("Control (MFO 100)",
                                                   "ACFO 50 / MFO 50", 
                                                   "ACFO 100"),
                           labels=c("Control (MFO 100)",
                                     "ICFO 50 / MFO 50", 
                                     "ICFO 100"))

#linear model + contrasts for IFW (initial fish weight)
ifw.2.lm <- lm(IFW~diet.name,stock2)
plot(ifw.2.lm)
summary(ifw.2.lm)
Anova(ifw.2.lm,type="II") # F = 2.3075 , P = 0.1806
(ifw.2.emm <- emmeans(ifw.2.lm, "diet.name"))
ifw.2.emm.df<-data.frame(ifw.2.emm)
contrast(ifw.2.emm, conts2,adjust="sidak")

ggplot(data=ifw.2.emm.df,
       aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),
                width=.1,color="black")+
  geom_point(shape=21,size=7,stroke=4)+
  xlab("")+
  ylab("Initial Fish Weight (g/fish)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#linear model + contrasts for FW (final fish weight)
fw.2.lm <- lm(w8.weight~diet.name,stock2)
plot(fw.2.lm)
summary(fw.2.lm)
Anova(fw.2.lm,type="II") # F = 0.6042 , P = 0.5767
(fw.2.emm <- emmeans(fw.2.lm, "diet.name"))
fw.2.emm.df<-data.frame(fw.2.emm)
contrast(fw.2.emm, conts2,adjust="sidak")

ggplot(data=fw.2.emm.df,
       aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),
                width=.1,color="black")+
  geom_point(shape=21,size=7,stroke=4)+
  xlab("")+
  ylab("Final Fish Weight (g/fish)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))


#making linear model for total.g.gain in stockoutcalculations.2.r
total.g.gain.2.lm <-lm(total.g.gain~diet.name,stock2)
plot(total.g.gain.2.lm)
summary(total.g.gain.2.lm)
Anova(total.g.gain.2.lm,type="II")
(total.g.gain.2.emm <- emmeans(total.g.gain.2.lm, "diet.name"))
total.g.gain.2.emm.df<-data.frame(total.g.gain.2.emm)
contrast(total.g.gain.2.emm, conts2,adjust="sidak")

ggplot(data=total.g.gain.2.emm.df,
       aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),
                width=.1,color="black")+
  geom_point(shape=21,size=7,stroke=4)+
  xlab("")+
  ylab("Growth (g per fish)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))+
  scale_fill_manual(values=colrs)+
  scale_color_manual(values=ocolrs)

ggsave("figures/trial2_growth.png",width=5.99,height=4.25)


#making linear model for avgFCR.all in stockoutcalculations.2.r
avgFCR.all.2.lm <-lm(avgFCR.all~diet.name,stock2)
plot(avgFCR.all.2.lm)
summary(avgFCR.all.2.lm)
anova(avgFCR.all.2.lm)
(avgFCR.all.2.emm <- emmeans(avgFCR.all.2.lm, "diet.name"))
avgFCR.all.2.emm.df<-data.frame(avgFCR.all.2.emm)
contrast(avgFCR.all.2.emm, conts2,adjust="sidak")

ggplot(data=avgFCR.all.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Average Feed Conversion Ratio for 8 Week Trial")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for avgFI.all in stockoutcalculations.2.r
avgFI.all.2.lm <-lm(avgFI.all~diet.name,stock2)
plot(avgFI.all.2.lm)
summary(avgFI.all.2.lm)
anova(avgFI.all.2.lm) # F = 0.5566 , P = 0.6002
(avgFI.all.2.emm <- emmeans(avgFI.all.2.lm, "diet.name"))
avgFI.all.2.emm.df<-data.frame(avgFI.all.2.emm)
contrast(avgFI.all.2.emm, conts2,adjust="sidak")

ggplot(data=avgFI.all.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Total Average Feed Intake for 8 Week Trial")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#making linear model for FCR.all in stockoutcalculations.2.r
FCR.all.2.lm <-lm(FCR.all~diet.name,stock2)
plot(FCR.all.2.lm)
summary(FCR.all.2.lm)
anova(FCR.all.2.lm) # F = 1.7236 , P = 0.2562
(FCR.all.2.emm <- emmeans(FCR.all.2.lm, "diet.name"))
FCR.all.2.emm.df<-data.frame(FCR.all.2.emm)
contrast(FCR.all.2.emm, conts2,adjust="sidak")

ggplot(data=FCR.all.2.emm.df,
       aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),
                width=.1,color="black")+
  geom_point(shape=21,size=10,stroke=4)+
  xlab("")+
  ylab("Feed Conversion Ratio")+
  theme(legend.position = "none",
        axis.text = element_text(size=24,color="black"),
        axis.title.y = element_text(size=24))+
  scale_fill_manual(values=colrs)+
  scale_color_manual(values=ocolrs)

ggsave("figures/trial2_FCR.png",width=11,height=5)

#making linear model for pincrease.all in stockoutcalculations.2.r
pincrease.all.2.lm <-lm(pincrease.all~diet.name,stock2)
plot(pincrease.all.2.lm)
summary(pincrease.all.2.lm)
anova(pincrease.all.2.lm) # F = 0.6713 , P = 0.5456
(pincrease.all.2.emm <- emmeans(pincrease.all.2.lm, "diet.name"))
pincrease.all.2.emm.df<-data.frame(pincrease.all.2.emm)
contrast(pincrease.all.2.emm, conts2,adjust="sidak")

ggplot(data=pincrease.all.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Total % Biomass Increase (g) for 8 Week Trial")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

