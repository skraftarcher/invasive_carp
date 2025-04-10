#Proximate Analysis
#AHost 3/19/2025

# load packages----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(readxl))install.packages("readxl");library(readxl)

# bring in data----
source("scripts/download_data-EX.R")

prox.cat <- read.csv("odata/catfish_prox.csv")

prox.cat<-prox.cat%>%
  mutate(protein.ww=round(Protein*(1-(moisture../100)+(Moisture/100)),2),
         fat.ww=round(Fat*(1-(moisture../100)+(Moisture/100)),2),
         fiber.ww=round(Fiber*(1-(moisture../100)+(Moisture/100)),2),
         ash.ww=round(Ash*(1-(moisture../100)+(Moisture/100)),2))

#add diets to respective trials
trials<-prox.cat%>%
  filter(season %in% c("Trial1","Trial2"))

trials <- trials%>%
  rename(ID = season)

#### Now analyze proximates with the "trials" data frame
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

tapply(trial1$moisture.., trial1$diet.name, sd)

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

tapply(trial2$moisture.., trial2$diet.name, sd) #standard deviation calcualtions

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
  geom_point(size=10,shape=21,stroke=4,aes(x=diet.name,y=emmean, color=diet.name,fill=diet.name))+
  xlab("")+
  ylab("Protein Content")+
  geom_errorbar(aes(x=diet.name,ymin=lower.CL,ymax=upper.CL),width=.1)+
  scale_color_manual(values=ocolrs)+
  scale_fill_manual(values=colrs)+
  theme(legend.position = "none",
        axis.text = element_text(size=24, color="black"),
        axis.title.y = element_text(size=24))

ggsave("figures/trial1_protein.png",width=10,height=5)

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



#### ENERGY ####

#load energy data from UL
energy <- read.csv("odata/catfish_energy.csv")
energy.trials <- energy%>%
  filter(season %in% c("Trial1","Trial2"))

energy.trials <- energy.trials %>%
  rename(Sample = ID)%>%
  rename(ID = season)

#### Now analyze proximates with the "trials" data frame

#establishing contrasts
conts<-list(control.all=c(1,0,-1),
            control.formfm=c(1,-1,0),
            formfm.all=c(0,1,-1))

energy.trials$diet.name <- factor(energy.trials$diet.name,levels=c("Control (100% MFM)",
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

energy.trial1 <- energy.trials[row.names(energy.trials)%in% 1:10,]
energy.trial2 <- energy.trials[row.names(energy.trials)%in%11:20,]

energy.trial1$heat.kcal_g <- energy.trial1$heat.cal_g / 1000
energy.trial2$heat.kcal_g <- energy.trial2$heat.cal_g / 1000


#contrasts for energy, trial 1
library(emmeans)
energy.1.lm <- lm(heat.kcal_g~diet.name,energy.trial1)
plot(energy.1.lm)
summary(energy.1.lm)
anova(energy.1.lm)
(energy.1.lm.emm <- emmeans(energy.1.lm, "diet.name"))
energy.1.lm.emm.df<-data.frame(energy.1.lm.emm)
contrast(energy.1.lm.emm, conts,adjust="sidak")

tapply(energy.trial1$heat.kcal_g, energy.trial1$diet.name, sd)

ggplot(data=energy.1.lm.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Energy kcal/g")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)

#contrasts for energy, trial 2
energy.2.lm <- lm(heat.kcal_g~diet.name,energy.trial2)
plot(energy.2.lm)
summary(energy.2.lm)
anova(energy.2.lm)
(energy.2.lm.emm <- emmeans(energy.2.lm, "diet.name"))
energy.2.lm.emm.df<-data.frame(energy.2.lm.emm)
contrast(energy.2.lm.emm, conts,adjust="sidak")

tapply(energy.trial2$heat.kcal_g, energy.trial2$diet.name, sd)

ggplot(data=energy.2.lm.emm.df,aes(x=diet.name,y=emmean))+
  geom_point()+
  xlab("")+
  ylab("Energy kcal/g")+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.1)
