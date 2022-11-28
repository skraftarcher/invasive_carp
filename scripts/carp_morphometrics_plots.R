# Script to pull in carp length/weight data

# install the package we'll use
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

season.colrs<-data.frame(Season=c("Winter","Spring","Summer","Fall"),
                         sab=c("WI","SP","SU","FA"),
                         colr=c("#af8137","#bdf4de","#0da426","#f59220"))

# pull the water quality data from google drive

lw<-read.csv("odata/carp_morpho.csv")[-1:-3,]%>%
  separate(Sample,sep="_",into=c("ID","Zone","Season"),convert=TRUE)%>%
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

# lipid data
library("readxl")
lipids<-read_xlsx("odata/Winter Carp Moisture and Fat measurements-2.xlsx",sheet = "forR")%>%
  group_by(ID)%>%
  summarize(wet.wt.poil=mean(wet.wt.poil),
            dry.wt.poil=mean(dry.wt.poil))%>%
  left_join(lw)


theme_set(theme_bw()+
            theme(panel.grid = element_blank(),
                  axis.text=element_text(size=14),
                  axis.title = element_text(size=16)))

# look at data-
(lplot<-ggplot(lw.sum,aes(x=Site,y=m.length,fill=Season))+
    geom_bar(stat="identity",alpha=.5,position=position_dodge())+
    geom_errorbar(aes(ymin=m.length-sd.length,ymax=m.length+sd.length),
                  position=position_dodge(0.9),width=.15)+
    ylab("Standard length (cm)")+
    xlab("")+
    scale_fill_manual(values=season.colrs$colr))


ggsave("figures/standard_length_plot.jpg",dpi=500)

(wplot<-ggplot(lw.sum,aes(x=Site,y=m.weight,fill=Season))+
  geom_bar(stat="identity",alpha=.5,position=position_dodge())+
  geom_errorbar(aes(ymin=m.weight-sd.weight,ymax=m.weight+sd.weight),
                position=position_dodge(0.9),width=.15)+
  ylab("Weight (g)")+
    xlab("")+
    scale_fill_manual(values=season.colrs$colr))

wplot

ggsave("figures/weight_plot.jpg",dpi=500)


(kplot<-ggplot(lw.sum,aes(x=Site,y=m.condition,fill=Season))+
  geom_bar(stat="identity",alpha=.5,position=position_dodge())+
  geom_errorbar(aes(ymin=m.condition-sd.condition,ymax=m.condition+sd.condition),
                position=position_dodge(0.9),width=.15)+
  ylab("Condition (K)")+
    xlab("")+
    scale_fill_manual(values=season.colrs$colr))

kplot

ggsave("figures/condition_plot.jpg",dpi=500)

(lwplot<-ggplot(lw,aes(x=standard.length,y=weight))+
  geom_point(aes(color=Site,shape=Season),alpha=.5,size=3)+
  scale_color_viridis_d(end=.8,guide=guide_legend(position=c(.2,.8)))+
  scale_shape(labels=c("Winter","Spring","Summer","Fall"))+
  ylab("Weight (g)")+
  xlab("Standard length (cm)"))

ggsave("figures/length_weight_plot.jpg",dpi=500,width=5.99,height=4.25)


# lipids by collection site
(lipplot<-ggplot(data=lipids)+
    geom_boxplot(aes(y=wet.wt.poil,fill=Site))+
    scale_fill_viridis_d(end=.8,
                         guide=guide_legend(position=c(.2,.8)))+
    ylab("% Lipids (wet weight)")+
    theme(axis.text.x = element_blank(),
          legend.title=element_blank(),
          legend.text=element_text(size=12)))

ggsave("figures/percent_lipids_wetweight.jpg")

(lwlipids<-ggplot(data=lipids)+
    geom_point(aes(x=standard.length,
                   y=wet.wt.poil,size=weight.kg,color=Site),
               alpha=.4)+
    scale_size(name="Weight (kg)")+
    scale_color_viridis_d(end=.8,
                         guide=guide_legend(position=c(.2,.8)))+
    ylab("% Lipids (wet weight)")+
    xlab("Standard length (cm)")+
    theme(legend.text = element_text(size=12)))
  
ggsave("figures/poilbystandardlength.jpg")
