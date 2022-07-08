# This is a script to look at whether or not 
# we need to keep doing water quality in all 
# of the tanks or if we can just do sump

# Written by Stephanie K. Archer 4/15/2022

# load packages

# install the package we'll use
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)

# pull the water quality data from google drive

wqurl<-"https://docs.google.com/spreadsheets/d/1z7UvXt1uBYFgw5rn5SwytC-8Q0md_fNve3T95GZyi4c/edit#gid=1866362456"

wq<-read.csv(text=gsheet2text(wqurl,format='csv'),
           stringsAsFactors = FALSE)%>%
  filter(!is.na(pH))%>%
  filter(!is.na(Ammonia))


# reorganize data so that the sump measurement from each date
# salinity dataset

sal<-wq%>%
  select(Date,Tank,Salinity)%>%
  filter(!is.na(Salinity))%>%
  pivot_wider(names_from = "Tank",values_from = "Salinity")%>%
  pivot_longer(`1`:`9`,names_to="Tank",values_to="Salinity")%>%
  mutate(sdiff=Salinity-sump)

theme_set(theme_bw())
# make plot to look at relationship between tank values and sump
ggplot(data=sal,aes(x=Date,y=sdiff,group=Tank,color=Tank))+
  geom_point()+
  theme(panel.grid=element_blank())+
  geom_hline(aes(yintercept=0),linetype="dashed",color="grey",alpha=.75)+
  geom_smooth(method="lm")+
  facet_wrap(~Tank)+
  ylab("Difference in Salinity (Tank-Sump")


# pH dataset

ph<-wq%>%
  select(Date,Tank,pH)%>%
  filter(!is.na(pH))%>%
  pivot_wider(names_from = "Tank",values_from = "pH")%>%
  pivot_longer(`1`:`9`,names_to="Tank",values_to="pH")%>%
  mutate(sdiff=pH-sump)

# make plot to look at relationship between tank values and sump
ggplot(data=ph,aes(x=Date,y=sdiff,group=Tank,color=Tank))+
  geom_point()+
  theme(panel.grid=element_blank())+
  geom_hline(aes(yintercept=0),linetype="dashed",color="grey",alpha=.75)+
  geom_smooth(method="lm")+
  facet_wrap(~Tank)+
  ylab("Difference in pH (Tank-Sump")

# Ammonia dataset

nh4<-wq%>%
  select(Date,Tank,Ammonia)%>%
  filter(!is.na(Ammonia))%>%
  mutate(Ammonia=as.numeric(Ammonia))%>%
  pivot_wider(names_from = "Tank",values_from = "Ammonia")%>%
  pivot_longer(`1`:`9`,names_to="Tank",values_to="Ammonia")%>%
  mutate(sdiff=Ammonia-sump)

# make plot to look at relationship between tank values and sump
ggplot(data=nh4,aes(x=Date,y=sdiff,group=Tank,color=Tank))+
  geom_point()+
  theme(panel.grid=element_blank())+
  geom_hline(aes(yintercept=0),linetype="dashed",color="grey",alAmmoniaa=.75)+
  geom_smooth(method="lm")+
  facet_wrap(~Tank)+
  ylab("Difference in Ammonia (Tank-Sump")


# DO dataset

do<-wq%>%
  select(Date,Tank,DO)%>%
  filter(!is.na(DO))%>%
  pivot_wider(names_from = "Tank",values_from = "DO")%>%
  pivot_longer(`1`:`9`,names_to="Tank",values_to="DO")%>%
  mutate(sdiff=DO-sump)

# make plot to look at relationship between tank values and sump
ggplot(data=do,aes(x=Date,y=sdiff,group=Tank,color=Tank))+
  geom_point()+
  theme(panel.grid=element_blank())+
  geom_hline(aes(yintercept=0),linetype="dashed",color="grey",alDOa=.75)+
  geom_smooth(method="lm")+
  facet_wrap(~Tank)+
  ylab("Difference in DO (Tank-Sump")


# DO dataset

alk<-wq%>%
  select(Date,Tank,Alkalinity)%>%
  filter(!is.na(Alkalinity))%>%
  pivot_wider(names_from = "Tank",values_from = "Alkalinity")%>%
  pivot_longer(`1`:`9`,names_to="Tank",values_to="Alkalinity")%>%
  mutate(sdiff=Alkalinity-sump)

# make plot to look at relationship between tank values and sump
ggplot(data=alk,aes(x=Date,y=sdiff,group=Tank,color=Tank))+
  geom_point()+
  theme(panel.grid=element_blank())+
  geom_hline(aes(yintercept=0),linetype="dashed",color="grey",alpha=.75)+
  geom_smooth(method="lm")+
  facet_wrap(~Tank)+
  ylab("Difference in Alkalinity (Tank-Sump")
