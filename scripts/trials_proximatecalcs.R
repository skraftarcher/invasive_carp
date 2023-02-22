#Trial 1 & 2 Proximate Calculations
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

#theme & colors
theme_set(theme_bw()+theme(panel.grid = element_blank()))
colrs<-c("#70AD47","#4A91D0","#4A91D0")
ocolrs<-c("#FFC000","#FFC000","#4A91D0")

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
theme_set(theme_bw()+theme(panel.grid = element_blank()))
colrs<-c("#70AD47","#4A91D0","#4A91D0")
ocolrs<-c("#FFC000","#FFC000","#4A91D0")


#split up trial 1 and trial 2 data into 2 data frames
trial1 <- trials[row.names(trials)%in% 1:10,]
trial2 <- trials[row.names(trials)%in%11:20,]


#contrasts for moisture, trial 1
moisture.ww.1.lm<- lm(moisture..~diet.name,trial1)
plot(moisture.ww.1.lm)
summary(moisture.ww.1.lm)
anova(moisture.ww.1.lm)
(moisture.ww.1.emm<-emmeans(moisture.ww.1.lm,"diet.name"))
moisture.ww.1.emm.df<-data.frame(moisture.ww.1.emm)
contrast(moisture.ww.1.emm,conts,adjust = "sidak")

ggplot(data=moisture.ww.1.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Moisture Content")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for moisture, trial 2
moisture.ww.2.lm<- lm(moisture..~diet.name,trial2)
plot(moisture.ww.2.lm)
summary(moisture.ww.2.lm)
anova(moisture.ww.2.lm)
(moisture.ww.2.emm<-emmeans(moisture.ww.2.lm,"diet.name"))
moisture.ww.2.emm.df<-data.frame(moisture.ww.2.emm)
contrast(moisture.ww.2.emm,conts,adjust = "sidak")

ggplot(data=moisture.ww.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Moisture Content")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for protein, trial 1
protein.ww.1.lm <- lm(protein.ww~diet.name,trial1)
plot(protein.ww.1.lm)
summary(protein.ww.1.lm)
anova(protein.ww.1.lm)
(protein.ww.1.emm <- emmeans(protein.ww.1.lm, "diet.name"))
protein.ww.1.emm.df<-data.frame(protein.ww.1.emm)
contrast(protein.ww.1.emm, conts,adjust="sidak")

ggplot(data=protein.ww.1.emm.df)+
  geom_point(size=10,shape=21,stroke=2,aes(x=diet.name,y=emmean, color=diet.name,fill=diet.name))+
  xlab("")+
  ylab("Protein Content")+
  geom_errorbar(aes(x=diet.name,ymin=lower.CL,ymax=upper.CL),width=.1)+
  scale_color_manual(values=ocolrs)+
  scale_fill_manual(values=colrs)+
  theme(legend.position = "none",
        axis.text = element_text(size=24, color="black"),
        axis.title.y = element_text(size=24))

#contrasts for protein, trial 2
colrs<-c("#70AD47","#4A91D0","#4A91D0")
ocolrs<-c("#70AD47","#70AD47","#4A91D0")


protein.ww.2.lm <- lm(protein.ww~diet.name,trial2)
plot(protein.ww.2.lm)
summary(protein.ww.2.lm)
anova(protein.ww.2.lm)
(protein.ww.2.emm <- emmeans(protein.ww.2.lm, "diet.name"))
protein.ww.2.emm.df<-data.frame(protein.ww.2.emm)
contrast(protein.ww.2.emm, conts,adjust="sidak")

ggplot(data=protein.ww.2.emm.df)+
  geom_errorbar(aes(x=diet.name,ymin=lower.CL,ymax=upper.CL),width=.1)+
  geom_point(size=10,shape=21,stroke=4,aes(x=diet.name,y=emmean, color=diet.name,fill=diet.name))+
  xlab("")+
  ylab("Protein Content")+
  scale_color_manual(values=ocolrs)+
  scale_fill_manual(values=colrs)+
  theme(legend.position = "none",
        axis.text = element_text(size=24, color="black"),
        axis.title.y = element_text(size=24))

ggsave("figures/trial2_protein.png",width=10.5,height=7)

#contrasts for fat, trial 1
fat.ww.1.lm <- lm(fat.ww~diet.name,trial1)
plot(fat.ww.1.lm)
summary(fat.ww.1.lm)
anova(fat.ww.1.lm)
(fat.ww.1.emm <- emmeans(fat.ww.1.lm, "diet.name"))
fat.ww.1.emm.df<-data.frame(fat.ww.1.emm)
contrast(fat.ww.1.emm, conts,adjust="sidak")

ggplot(data=fat.ww.1.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Fat Content")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for fat, trial 2
fat.ww.2.lm <- lm(fat.ww~diet.name,trial2)
plot(fat.ww.2.lm)
summary(fat.ww.2.lm)
anova(fat.ww.2.lm)
(fat.ww.2.emm <- emmeans(fat.ww.2.lm, "diet.name"))
fat.ww.2.emm.df<-data.frame(fat.ww.2.emm)
contrast(fat.ww.2.emm, conts,adjust="sidak")

ggplot(data=fat.ww.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Fat Content")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for fiber, trial 1
fiber.ww.1.lm <- lm(fiber.ww~diet.name,trial1)
plot(fiber.ww.1.lm)
summary(fiber.ww.1.lm)
anova(fiber.ww.1.lm)
(fiber.ww.1.emm <- emmeans(fiber.ww.1.lm, "diet.name"))
fiber.ww.1.emm.df<-data.frame(fiber.ww.1.emm)
contrast(fiber.ww.1.emm, conts,adjust="sidak")

ggplot(data=fiber.ww.1.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Fiber Content")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for fiber, trial 2
fiber.ww.2.lm <- lm(fiber.ww~diet.name,trial2)
plot(fiber.ww.2.lm)
summary(fiber.ww.2.lm)
anova(fiber.ww.2.lm)
(fiber.ww.2.emm <- emmeans(fiber.ww.2.lm, "diet.name"))
fiber.ww.2.emm.df<-data.frame(fiber.ww.2.emm)
contrast(fiber.ww.2.emm, conts,adjust="sidak")

ggplot(data=fiber.ww.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Fiber Content")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for ash, trial 1
ash.ww.1.lm <- lm(ash.ww~diet.name,trial1)
plot(ash.ww.1.lm)
summary(ash.ww.1.lm)
anova(ash.ww.1.lm)
(ash.ww.1.emm <- emmeans(ash.ww.1.lm, "diet.name"))
ash.ww.1.emm.df<-data.frame(ash.ww.1.emm)
contrast(ash.ww.1.emm, conts,adjust="sidak")

ggplot(data=ash.ww.1.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Ash Content")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for ash, trial 2
ash.ww.2.lm <- lm(ash.ww~diet.name,trial2)
plot(ash.ww.2.lm)
summary(ash.ww.2.lm)
anova(ash.ww.2.lm)
(ash.ww.2.emm <- emmeans(ash.ww.2.lm, "diet.name"))
ash.ww.2.emm.df<-data.frame(ash.ww.2.emm)
contrast(ash.ww.2.emm, conts,adjust="sidak")

ggplot(data=ash.ww.2.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Ash Content")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)
