# Script to pull in carp length/weight data

# install the package we'll use
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)

season.colrs<-data.frame(Season=c("Winter","Spring","Summer","Fall"),
                         sab=c("WI","SP","SU","FA"),
                         colr=c("#af8137","#bdf4de","#0da426","#f59220"))

# pull the water quality data from google drive
lwurl<-"https://docs.google.com/spreadsheets/d/1xcvCo-l4kEAHMCObgjmXTvHejY3oh27Hv_XTF-3Iun8/edit?usp=sharing"

lw<-read.csv(text=gsheet2text(lwurl,format='csv'),
             stringsAsFactors = FALSE)[-1:-3,]%>%
  separate(ID,sep="_",into=c("ID","Zone","Season"))%>%
  mutate(Site=case_when(
    Zone=="0101"~"Louisiana \n Zone 0101",
    Zone=="0103"~"Louisiana \n Zone 0103",
    Zone=="IL"~"Illinois" ),
    condition=(100*weight)/(standard.length^3),
    Season=factor(Season,levels=season.colrs$sab))

lw.sum<-lw%>%
  group_by(Site,Season)%>%
  summarize(m.length=mean(standard.length),
            sd.length=sd(standard.length),
            m.weight=mean(weight),
            sd.weight=sd(weight),
            m.condition=mean(condition),
            sd.condition=sd(condition))

theme_set(theme_bw()+
            theme(panel.grid = element_blank(),
                  axis.text=element_text(size=14),
                  axis.title = element_text(size=16)))

# look at data
(lplot<-ggplot(lw.sum,aes(x=Site,y=m.length,fill=Season))+
    geom_bar(stat="identity",alpha=.5,position=position_dodge())+
    geom_errorbar(aes(ymin=m.length-sd.length,ymax=m.length+sd.length),
                  position=position_dodge(0.9),width=.15)+
    ylab("Standard length (cm)")+
    xlab("")+
    scale_fill_manual(values=season.colrs$colr[1:2]))


ggsave("figures/standard_length_plot.jpg",dpi=500)

(wplot<-ggplot(lw.sum,aes(x=Site,y=m.weight,fill=Season))+
  geom_bar(stat="identity",alpha=.5,position=position_dodge())+
  geom_errorbar(aes(ymin=m.weight-sd.weight,ymax=m.weight+sd.weight),
                position=position_dodge(0.9),width=.15)+
  ylab("Weight (g)")+
    xlab("")+
    scale_fill_manual(values=season.colrs$colr[1:2]))

wplot

ggsave("figures/weight_plot.jpg",dpi=500)


(kplot<-ggplot(lw.sum,aes(x=Site,y=m.condition,fill=Season))+
  geom_bar(stat="identity",alpha=.5,position=position_dodge())+
  geom_errorbar(aes(ymin=m.condition-sd.condition,ymax=m.condition+sd.condition),
                position=position_dodge(0.9),width=.15)+
  ylab("Condition (K)")+
    xlab("")+
    scale_fill_manual(values=season.colrs$colr[1:2]))

kplot

ggsave("figures/condition_plot.jpg",dpi=500)

(lwplot<-ggplot(lw,aes(x=standard.length,y=weight))+
  geom_point(aes(color=Site,shape=Season),alpha=.5,size=3)+
  scale_color_viridis_d(end=.8,guide=guide_legend(position=c(.2,.8)))+
    scale_shape(labels=c("Winter","Spring"))+
  ylab("Weight (g)")+
  xlab("Standard length (cm)"))

ggsave("figures/length_weight_plot.jpg",dpi=500,width=5.99,height=4.25)
