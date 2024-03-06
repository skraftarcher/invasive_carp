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
theme_set(theme_bw()+theme(panel.grid = element_blank()))

#pull the samplingcalculations.R and stockoutcaculations.R data from ACFM Trial 1 google drive
source("scripts/download_data-EX.R")

samp <- read.csv("odata/trial1_samplingcalcs.csv")
stock <- read.csv("odata/trial1_stockingcalcs.csv")

par(mfrow=c(2,2))
# generate list of contrasts that we want to do
conts<-list(control.all=c(1,0,-1),
            control.formfm=c(1,-1,0),
            formfm.all=c(0,1,-1))#,
#carp.nocarp=c(-2,1,1),
#p15.p100=c(1,1,-2))

#SAMPLING DATA CALCULATIONS
#organize data a  bit
samp$diet <- factor(samp$diet,levels=c(1,2,3))
samp$diet.name <- factor(samp$diet.name,levels=c("Control (100% MFM)",
                                                 "ACFM for MFM", 
                                                 "All ACFM"),
                         labels = c("Control (100% MFM)",
                                    "ICFM for MFM", 
                                    "All ICFM"))
colrs<-c("#70AD47","#4A91D0","#4A91D0")
ocolrs<-c("#FFC000","#FFC000","#4A91D0")


#making linear model for HSI in samplingcalculations

HSI.lm <- lmer(HSI~diet.name+(1|tank),samp)
plot(HSI.lm)
summary(HSI.lm)
anova(HSI.lm)
#in summary, all ACFM is significantly different from control for HSI, but in anova, diet does not significantly affect HSI

# working through contrasts
(HSI.emm <- emmeans(HSI.lm, "diet.name"))
HSI.emm.df<-data.frame(HSI.emm)
contrast(HSI.emm, conts,adjust="sidak")

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
contrast(VSI.emm, conts,adjust="sidak")

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
contrast(MR.emm, conts,adjust="sidak")

#visualizing MR contrast results
ggplot(data=MR.emm.df)+
  geom_errorbar(aes(x=diet.name,ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=10,shape=21,stroke=4,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  #geom_boxplot(aes(x=diet.name,y=MR),alpha=.2,data=samp)
  xlab("")+
  ylab("Muscle Ratio")+
  scale_color_manual(values=ocolrs)+
  scale_fill_manual(values=colrs)+
  theme(legend.position = "none",
        axis.text = element_text(size=24, color="black"),
        axis.title.y = element_text(size=24))

ggsave("figures/trial1_MR.png",width=10,height=5)
#making linear model for cond.factor in sampling calculations

cf.lm <-lmer(cond.factor~diet.name+(1|tank),samp)
plot(cf.lm)
summary(cf.lm)
anova(cf.lm)
#contrasts for condition factor
(cf.emm<-emmeans(cf.lm,"diet.name"))
cf.emm.df<-data.frame(cf.emm)
contrast(cf.emm, conts,adjust="sidak")

#visualizing condition factor contrast results
ggplot(data=cf.emm.df,aes(x=diet.name,y=emmean,
                          fill=diet.name,color=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),color="black",width=.1)+
  geom_point(size=5,shape=21,stroke=2)+
  xlab("")+
  ylab("Condition Factor")+
  scale_color_manual(values=ocolrs)+
  scale_fill_manual(values=colrs)+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))


ggsave("figures/trial1_CF.png",width=5.99,height=4.25)

#STOCKOUT DATA CALCULATIONS
#reorganize data
stock$diet <- factor(stock$diet,levels=c(1,2,3))
stock$diet.name <- factor(stock$diet.name,levels=c("Control (100% MFM)",
                                                   "ACFM for MFM", 
                                                   "All ACFM"),
                          labels=c("Control (100% MFM)",
                                   "ICFM for MFM", 
                                   "All ICFM"))


#going to use emmeans for contrasts b/c it is the suggested new version of lsmeans, which is the contrast package used on the R walkthrough


#linear model + contrasts for IFW (Initial Fish Weight)
ifw.lm <- lm(IFW~diet.name, stock)
plot(ifw.lm)
summary(ifw.lm)
Anova(ifw.lm,type="II") #df 2, residuals df 6, F-value = 1.8372, P=0.2385
(ifw.emm<-emmeans(ifw.lm,"diet.name"))
ifw.emm.df<-data.frame(ifw.emm)
contrast(ifw.emm,conts,adjust="sidak")

ggplot(data=ifw.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Fish Weight (g/fish)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEKS 0-4: w4 weight 
halfw.lm <- lm(w4.weight~diet.name, stock)
plot(halfw.lm)
summary(halfw.lm)
Anova(halfw.lm,type="II") # F = 1.0184 , P = 0.4161
(halfw.emm<-emmeans(halfw.lm,"diet.name"))
halfw.emm.df<-data.frame(halfw.emm)
contrast(halfw.emm,conts,adjust="sidak")

ggplot(data=halfw.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Fish Weight (g/fish)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEKS 0-4: w0-4 PIncrease 
halfpi.lm <- lm(w0.4.pincrease~diet.name, stock)
plot(halfpi.lm)
summary(halfpi.lm)
Anova(halfpi.lm,type="II") # F = 0.2992 , P = 0.7519
(halfpi.emm<-emmeans(halfpi.lm,"diet.name"))
halfpi.emm.df<-data.frame(halfpi.emm)
contrast(halfpi.emm,conts,adjust="sidak")

ggplot(data=halfpi.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("% Biomass Increase (g)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEKS 0-4: w0-4 FI
halfFI.lm <- lm(w0.4.FI~diet.name, stock)
plot(halfFI.lm)
summary(halfFI.lm)
Anova(halfFI.lm,type="II") # F = 3.3579 , P = 0.1051
(halfFI.emm<-emmeans(halfFI.lm,"diet.name"))
halfFI.emm.df<-data.frame(halfFI.emm)
contrast(halfFI.emm,conts,adjust="sidak")

ggplot(data=halfFI.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Feed Intake")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEKS 0-4: w0-4 FCR
halfFCR.lm <- lm(w0.4.FCR~diet.name, stock)
plot(halfFCR.lm)
summary(halfFCR.lm)
Anova(halfFCR.lm,type="II") # F = 0.6574 , P = 0.5519
(halfFCR.emm<-emmeans(halfFCR.lm,"diet.name"))
halfFCR.emm.df<-data.frame(halfFCR.emm)
contrast(halfFCR.emm,conts,adjust="sidak")

ggplot(data=halfFCR.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Feed Conversion Ratio")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEKS 6-8: IFW at Week 6
w6fw.lm <- lm(w6.weight~diet.name, stock)
plot(w6fw.lm)
summary(w6fw.lm)
Anova(w6fw.lm,type="II") # F =  2.9926 , P = 0.1398
(w6fw.emm<-emmeans(w6fw.lm,"diet.name"))
w6fw.emm.df<-data.frame(w6fw.emm)
contrast(w6fw.emm,conts,adjust="sidak")

ggplot(data=w6fw.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Fish Weight")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEKS 6-8: FFW at Week 8
fw.lm <- lm(w8.weight~diet.name, stock)
plot(fw.lm)
summary(fw.lm)
Anova(fw.lm,type="II") # F = 3.6875 , P = 0.09027
(fw.emm<-emmeans(fw.lm,"diet.name"))
fw.emm.df<-data.frame(fw.emm)
contrast(fw.emm,conts,adjust="sidak")

ggplot(data=fw.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Fish Weight (g/fish)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEK 6-8: WG % Increase
w68pi.lm <- lm(w6.8.pincrease~diet.name, stock)
plot(w68pi.lm)
summary(w68pi.lm)
Anova(w68pi.lm,type="II") # F = 0.5705 , P = 0.5982
(w68pi.emm<-emmeans(w68pi.lm,"diet.name"))
w68pi.emm.df<-data.frame(w68pi.emm)
contrast(w68pi.emm,conts,adjust="sidak")

ggplot(data=w68pi.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Total % Biomass Increase (g)")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEK 6-8: FI
w68FI.lm <- lm(w6.8.FI~diet.name, stock)
plot(w68FI.lm)
summary(w68FI.lm)
Anova(w68FI.lm,type="II") # F = 2.9889 , P = 0.14
(w68FI.emm<-emmeans(w68FI.lm,"diet.name"))
w68FI.emm.df<-data.frame(w68FI.emm)
contrast(w68FI.emm,conts,adjust="sidak")

ggplot(data=w68FI.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Feed Intake")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#WEEK 6-8: FCR
w68FCR.lm <- lm(w6.8.FCR~diet.name, stock)
plot(w68FCR.lm)
summary(w68FCR.lm)
Anova(w68FCR.lm,type="II") # F = 4.7178 , P = 0.07061
(w68FCR.emm<-emmeans(w68FCR.lm,"diet.name"))
w68FCR.emm.df<-data.frame(w68FCR.emm)
contrast(w68FCR.emm,conts,adjust="sidak")

ggplot(data=w68FCR.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Feed Conversion Ratio")+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

#making linear model for total.g.gain in sampling calculations

total.g.gain.lm <-lm(total.g.gain~diet.name,stock)
plot(total.g.gain.lm)
summary(total.g.gain.lm)
Anova(total.g.gain.lm,type="II")

# working through contrasts
(total.g.gain.emm <- emmeans(total.g.gain.lm, "diet.name"))
total.g.gain.emm.df<-data.frame(total.g.gain.emm)
contrast(total.g.gain.emm, conts,adjust="sidak")

ggplot(data=total.g.gain.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=7,shape=21,stroke=2)+
  xlab("")+
  ylab("Growth (g per fish)")+
  scale_color_manual(values=ocolrs)+
  scale_fill_manual(values=colrs)+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=16))

ggsave("figures/trial1_growth.png",width=5.99,height=4.25)

#making linear model for avgFCR.all in sampling calculations

avgFCR.all.lm <-lm(avgFCR.all~diet.name,stock)
plot(avgFCR.all.lm)
summary(avgFCR.all.lm)
anova(avgFCR.all.lm)
#according to summary, the average feed conversion ratio for the ALL ACFM diet was significantly different (less than/smaller) than the control diet. The Anova however showed no significant differences in avg feed conversion ratio between diets
# working through contrasts 
(avgFCR.all.emm <- emmeans(avgFCR.all.lm, "diet.name"))
avgFCR.all.emm.df<-data.frame(avgFCR.all.emm)
contrast(avgFCR.all.emm, conts,adjust="sidak")

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
contrast(avgFI.all.emm, conts,adjust="sidak")

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
#The summary shows a significant difference in total feed conversion ratio for the ALL ACFM, such that is less than the FCR for the control diet. However, ANova shows no significant variance between the 3 diets and FCR
# working through contrasts
(FCR.all.emm <- emmeans(FCR.all.lm, "diet.name"))
FCR.all.emm.df<-data.frame(FCR.all.emm)
contrast(FCR.all.emm, conts,adjust="sidak")

ggplot(data=FCR.all.emm.df,aes(x=diet.name,y=emmean,color=diet.name,fill=diet.name))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1,color="black")+
  geom_point(size=10,shape=21,stroke=4)+
  xlab("")+
  ylab("Feed Conversion Ratio")+
  scale_color_manual(values=ocolrs)+
  scale_fill_manual(values=colrs)+
  theme(legend.position = "none",
        axis.text = element_text(size=24,color="black"),
        axis.title.y = element_text(size=24))

ggsave("figures/trial1_fcrall.png",width=10,height=5)

#making linear model for pincrease.all in sampling calculations
pincrease.all.lm <-lm(pincrease.all~diet.name,stock)
plot(pincrease.all.lm)
summary(FCR.all.lm)
anova(pincrease.all.lm)
#Summary shows a significant difference for percent increase (total biomass increase) of the All ACFM tanks compared to the control diet. However, Anova shows no significant differences for % biomass increase for the 3 diets
# working through contrasts
(pincrease.all.emm <- emmeans(pincrease.all.lm, "diet.name"))
pincrease.all.emm.df<-data.frame(pincrease.all.emm)
contrast(pincrease.all.emm, conts,adjust="sidak")

ggplot(data=pincrease.all.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Total % Biomass Increase (g) for 8 Week Trial")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)


#comparing FI values from 0-4 weeks and 4-8 weeks

#reorganizing data
stock2 <- stock%>%
  select(diet,diet.name,tank,w0.4.FI,w4.8.FI,w6.8.FI)%>%
  pivot_longer(w0.4.FI:w6.8.FI,names_to = "mt",values_to = "FI")%>%
  filter(!is.na(FI))%>%
  mutate(nweeks=ifelse(mt=="w6.8.FI",2,4),
         nhalf=ifelse(mt=="w0.4.FI","First","Second"),
         perweekFI=FI/nweeks)

#model for comparing FI 
FI.lm <- lm(perweekFI~diet.name*nhalf,stock2)
plot(FI.lm)
summary(FI.lm)
anova(FI.lm)
#data shows an overall increase in FI per week for all tanks, and a significant increase in FI per week between the first half of the trial and the second half of the trial 
FI.lm.first <- lm(perweekFI~diet.name,stock2%>%
                    filter(nhalf=="First"))
plot(FI.lm.first)
summary(FI.lm.first)
anova(FI.lm.first)

