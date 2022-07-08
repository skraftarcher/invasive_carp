# Script to pull in carp length/weight data

# install the package we'll use
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)

# pull the water quality data from google drive
lwurl<-"https://docs.google.com/spreadsheets/d/1xcvCo-l4kEAHMCObgjmXTvHejY3oh27Hv_XTF-3Iun8/edit?usp=sharing"

lw<-read.csv(text=gsheet2text(lwurl,format='csv'),
             stringsAsFactors = FALSE)[-1:-3,]%>%
  separate(ID,sep="_",into=c("ID","Zone","Season"))%>%
  mutate(Site=case_when(
    Zone=="0101"~"Louisiana Zone 0101",
    Zone=="0103"~"Louisiana Zone 0103",
    Zone=="IL"~"Illinois" ),
    condition=(100*weight)/(standard.length^3))

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
                  axis.text=element_text(size=18),
                  axis.title = element_text(size=22)))

# look at data
lplot<-ggplot(lw.sum,aes(x=Site,y=m.length))+
  geom_bar(stat="identity",alpha=.5)+
  geom_errorbar(aes(ymin=m.length-sd.length,ymax=m.length+sd.length),position=position_dodge(0.9),width=.15)+
  ylab("Standard length (cm)")

lplot

ggsave("figures/standard_length_plot.jpg",dpi=500)

wplot<-ggplot(lw.sum,aes(x=Site,y=m.weight))+
  geom_bar(stat="identity",alpha=.5)+
  geom_errorbar(aes(ymin=m.weight-sd.weight,ymax=m.weight+sd.weight),position=position_dodge(0.9),width=.15)+
  ylab("Weight (g)")

wplot

ggsave("figures/weight_plot.jpg",dpi=500)


kplot<-ggplot(lw.sum,aes(x=Site,y=m.condition))+
  geom_bar(stat="identity",alpha=.5)+
  geom_errorbar(aes(ymin=m.condition-sd.condition,ymax=m.condition+sd.condition),position=position_dodge(0.9),width=.15)+
  ylab("Condition (K)")

kplot

ggsave("figures/condition_plot.jpg",dpi=500)

lwplot<-ggplot(lw,aes(x=standard.length,y=weight))+
  geom_point(aes(color=Site),alpha=.5,size=3)+
  scale_color_viridis_d(end=.8)+
  ylab("Weight (g)")+
  xlab("Standard length (cm)")+
  theme(legend.position = c(.2,.8))

lwplot

ggsave("figures/length_weight_plot.jpg",dpi=500)
