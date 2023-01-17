#proximate data
#Stephanie K. Archer 11/28/2022

# load packages----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(readxl))install.packages("readxl");library(readxl)

# bring in data----
source("scripts/download_data-EX.R")

prox.res<-read_xlsx("odata/22_14521LUMCONsa_ah.xlsx",sheet="forR")
sample.id<-read_xlsx("odata/22_14521 Archer Sample ID Key.xlsx",sheet="forR")
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
ms.ds<-left_join(prox.res,sample.id)%>%
  left_join(moist)%>%
  mutate(protein.ww=round(Protein*(1-(moisture../100)+(Moisture/100)),2),
         fat.ww=round(Fat*(1-(moisture../100)+(Moisture/100)),2),
         fiber.ww=round(Fiber*(1-(moisture../100)+(Moisture/100)),2),
         ash.ww=round(Ash*(1-(moisture../100)+(Moisture/100)),2))
  

# pull apart carp, diet, and feeding trial data----

carp<-ms.ds%>%
  filter(season %in% c("WI","SP","FA","SU"))%>%
  left_join(carp.morph)

trials<-ms.ds%>%
  filter(season %in% c("Trial1","Trial2"))%>%
  left_join(samp.ras)

diets<-ms.ds%>%
  filter(season %in% c("Diet1","Diet2","Diet3","Diet4","Diet5","Diet6"))%>%
  left_join(samp.diet)

# quickly visualize data----
ggplot(data=carp)+
  # geom_point(aes(x=weight.kg,y=protein.ww,color=area,shape=season),size=3,alpha=.5)+
  # geom_point(aes(x=weight.kg,y=fat.ww,color=area,shape=season),size=3,alpha=.5)+
  # geom_point(aes(x=weight.kg,y=fiber.ww,color=area,shape=season),size=3,alpha=.5)+
  # geom_point(aes(x=weight.kg,y=ash.ww,color=area,shape=season),size=3,alpha=.5)+
  # geom_boxplot(aes(y=protein.ww,fill=area))+
  # geom_boxplot(aes(y=fat.ww,fill=area))+
  # geom_boxplot(aes(y=fiber.ww,fill=area))+
  geom_boxplot(aes(y=ash.ww,fill=area))+
  facet_wrap(~season)+
  scale_color_viridis_d()

ggplot(data=trials)+
  geom_boxplot(aes(y=fat.ww,fill=diet.name))+
  facet_wrap(~season,scales="free")+
  scale_fill_viridis_d()
