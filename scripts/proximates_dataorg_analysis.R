#proximate data
#Stephanie K. Archer 11/28/2022

# load packages----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(readxl))install.packages("readxl");library(readxl)

# bring in data----
source("scripts/download_data-EX.R")

prox.1<-read.csv("odata/proxdata1.csv")%>%
  mutate(LAU.MC.ID=as.character(LAU.MC.ID))
prox.2<-read.csv("odata/proxdata2.csv")%>%
  mutate(LAU.MC.ID=as.character(LAU.MC.ID))
id1<-read.csv("odata/sampleIDs_prox1.csv")%>%
  select(LAU.MC.ID,Sample,Rep)%>%
  mutate(LAU.MC.ID=as.character(LAU.MC.ID))
id2<-read.csv("odata/sampleIDs_prox2.csv")%>%
  select(LAU.MC.ID,Sample)%>%
  mutate(Rep=1)%>%
  mutate(LAU.MC.ID=as.character(LAU.MC.ID))
carp.morph<-read.csv("odata/carp_morpho.csv")
moist<-read.csv("odata/moisture.csv")
samp1 <-read.csv("odata/trial1_samplingcalcs.csv")%>%
  select(diet.name,diet,tank)%>%
  distinct()%>%
  mutate(area=paste0("RAS",tank),
         season=paste0("Diet",diet))%>%
  select(diet.name,area,season)

samp2<-read.csv("odata/trial2_samplingcalcs.csv")%>%
  select(diet.name,diet,tank)%>%
  distinct()%>%
  mutate(area=paste0("RAS",tank),
         season=paste0("Diet",diet))%>%
  select(diet.name,area,season)

samp.ras1<-samp1%>%
  select(-season)%>%
  bind_rows(data.frame(diet.name="Pre-Trial",area="initial"))%>%
  mutate(season="Trial1")
  
samp.ras2<-samp2%>%
  select(-season)%>%
  bind_rows(data.frame(diet.name="Pre-Trial",area="initial"))%>%
  mutate(season="Trial2")

samp.ras<-bind_rows(samp.ras1,samp.ras2)

samp.diet<-bind_rows(samp1,samp2)%>%
  select(-area)%>%
  distinct()

# join data together----
ms.ds1<-left_join(prox.1,id1)%>%
  left_join(moist)%>%
  mutate(protein.ww=round(Protein*(1-(moisture../100)+(Moisture/100)),2),
         fat.ww=round(Fat*(1-(moisture../100)+(Moisture/100)),2),
         fiber.ww=round(Fiber*(1-(moisture../100)+(Moisture/100)),2),
         ash.ww=round(Ash*(1-(moisture../100)+(Moisture/100)),2))

ms.ds2<-left_join(prox.2,id2)%>%
  left_join(moist)%>%
  mutate(protein.ww=round(Protein*(1-(moisture../100)+(Moisture/100)),2),
         fat.ww=round(Fat*(1-(moisture../100)+(Moisture/100)),2),
         fiber.ww=round(Fiber*(1-(moisture../100)+(Moisture/100)),2),
         ash.ww=round(Ash*(1-(moisture../100)+(Moisture/100)),2))

ms.ds<-bind_rows(ms.ds1,ms.ds2)%>%
  separate(Sample, into=c("ID","Zone","Season"),sep="_",remove=FALSE)%>%
  select(-comments)

# pull apart carp, diet, and feeding trial data----

carp<-ms.ds%>%
  filter(Season %in% c("WI","SP","FA","SU"))%>%
  left_join(carp.morph)%>%
  mutate(State=ifelse(Zone=="IL","Illinois","Louisiana"))
carp$Season<-factor(carp$Season,levels=c("WI","SP","SU","FA"))

trials<-ms.ds%>%
  filter(ID %in% c("Trial1","Trial2"))%>%
  rename(season=ID)%>%
  left_join(samp.ras)

diets<-ms.ds%>%
  filter(ID %in% c("Diet1","Diet2","Diet3","Diet4","Diet5","Diet6"))%>%
  rename(season=ID)%>%
  left_join(samp.diet)

# quickly visualize data----
theme_set(theme_bw()+theme(panel.grid=element_blank()))

ggplot(data=carp)+
  # geom_point(aes(x=weight.kg,y=protein.ww,color=Zone,shape=State),size=3,alpha=.5)+
  # geom_point(aes(x=weight.kg,y=fat.ww,color=Zone),size=3,alpha=.5)+
   # geom_point(aes(x=weight.kg,y=fiber.ww,color=Zone),size=3,alpha=.5)+
  # geom_point(aes(x=weight.kg,y=ash.ww,color=Zone),size=3,alpha=.5)+
  # geom_boxplot(aes(y=protein.ww,fill=Zone))+
  # geom_boxplot(aes(y=fat.ww,fill=Zone))+
  # geom_boxplot(aes(y=fiber.ww,fill=Zone))+
  geom_boxplot(aes(y=ash.ww,fill=Zone))+
  facet_wrap(~Season)+
  scale_color_viridis_d()


# compare prox across zones
carp<-carp%>%
  group_by(Sample,ID,Zone,Season,State)%>%
  summarize(moisture=mean(moisture..,na.rm=TRUE),
            protein=mean(protein.ww,na.rm = TRUE),
            fat=mean(fat.ww,na.rm=TRUE),
            fiber=mean(fiber.ww,na.rm = TRUE),
            ash=mean(ash.ww,na.rm=TRUE))%>%
  left_join(carp.morph)

# does zone explain weight and length?
par(mfrow=c(2,2))
laov<-aov(standard.length~Zone,data=carp)
plot(laov)
summary(laov)
# yes
TukeyHSD(laov)
#fish are longest in  0101 followed by 0103 then IL - no significant difference
# between 0101 and 0103


waov<-aov(weight.kg~Zone,data=carp)
plot(waov)
summary(waov)
# yes
TukeyHSD(waov)
#fish are longest in  0101 followed by 0103 then IL 

# is the relationship between length and weight the same across zones?
lwlm<-lm(weight.kg~standard.length*Zone,data=carp)
plot(lwlm)
summary(lwlm)

int.exp=expression(paste("F"["2,89"],"= 18.93, p < 0.0001"))

# visualize this
ggplot()+
  geom_point(data=carp,aes(x=standard.length,y=weight.kg,color=Zone))+
  geom_smooth(aes(x=standard.length,y=weight.kg,color=Zone),method="lm",data=carp)+
  scale_color_viridis_d()+
  ylab("Weight (kg)")+
  xlab("Standard length (cm)")+
  #geom_text(aes(x=40,y=17),label="Length X Zone:")+
  #geom_text(aes(x=40,y=16),label=int.exp,parse=T)+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        legend.text = element_text(size=14),
        legend.title=element_text(size=18))

ggsave("figures/length_weight_relationship.jpg",dpi=500,height=5,width=7)


#protein
# visualize 
ggplot()+
  geom_point(data=carp,aes(x=weight.kg,y=protein,color=Zone))+
  geom_smooth(aes(x=weight.kg,y=protein,color=Zone),method="lm",data=carp)+
  scale_color_viridis_d()+
  facet_wrap(~Season)

ggplot()+
  geom_point(data=carp,aes(x=standard.length,y=protein,color=Zone))+
  geom_smooth(aes(x=standard.length,y=protein,color=Zone),method="lm",data=carp)+
  scale_color_viridis_d()+
  facet_wrap(~Season)

ggplot(data=carp)+
  geom_boxplot(aes(y=protein,fill=Season))+
  scale_fill_viridis_d()+
  facet_wrap(~Zone)

protein.full<-aov(protein~Zone*Season+standard.length+weight.kg,data=carp)
protein.seas<-aov(protein~Zone*Season,data=carp)
protein.length<-aov(protein~Zone+standard.length,data=carp)
protein.length.weight<-aov(protein~Zone+standard.length+weight.kg,data=carp)
protein.weight<-aov(protein~Zone+weight.kg,data=carp)
anova(protein.full,protein.seas,protein.length,protein.length.weight,protein.weight)
AIC(protein.full,protein.seas,protein.length,protein.length.weight,protein.weight)

# zone and season explain protein better than other models

summary(protein.seas)
TukeyHSD(protein.seas)

# fish from both Louisiana zones have more protein than those from Illinois
# fish from zone 0103 have slightly higher protein than those from 0101
# across zones fish have higher % protein content in the winter no other seasons are different than each other

#fat
fat.full<-aov(fat~Zone*Season+Zone*standard.length+Zone*weight.kg,data=carp)
fat.seas<-aov(fat~Zone*Season,data=carp)
fat.length<-aov(fat~Zone*standard.length,data=carp)
fat.length.weight<-aov(fat~Zone*standard.length+Zone*weight.kg,data=carp)
fat.weight<-aov(fat~Zone*weight.kg,data=carp)
anova(fat.full,fat.seas,fat.length,fat.length.weight,fat.weight)
AIC(fat.full,fat.seas,fat.length,fat.length.weight,fat.weight)

# fat is best explained by the zone weight model

plot(fat.weight)
summary(fat.weight)

# not suprisingly as fish weigh more they have more fat - but zone is still significant
# the interaction is not

TukeyHSD(fat.weight,which="Zone")

# all zomn

#fiber
fiber.aov<-aov(fiber~Zone*Season,data=carp)
plot(fiber.aov)
summary(fiber.aov)
TukeyHSD(fiber.aov)


#ash
ash.aov<-aov(ash~Zone*Season,data=carp)
plot(ash.aov)
summary(ash.aov)
TukeyHSD(ash.aov)

