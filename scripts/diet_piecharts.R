# remake pie charts
trial1<-data.frame(diet=rep(c("Control","ICFM for MFM","ICFM"),4),
                   protein=c(rep("Menhaden Fish Meal (MFM)",3),
                             rep("Invasive Carp Fish Meal (ICFM)",3),
                             rep("Soybean Meal",3),
                             rep("Corn Protein Concentrate",3)),
                   percent=c(30,0,0,0,31,100,64,63,0,6,6,0))
                   
trial1$diet<-factor(trial1$diet,levels=c("Control","ICFM for MFM","ICFM"))
trial1$protein<-factor(trial1$protein,
                       levels=c("Menhaden Fish Meal (MFM)",
                       "Invasive Carp Fish Meal (ICFM)",
                       "Soybean Meal",
                       "Corn Protein Concentrate"))

ggplot(data=trial1)+
  geom_bar(aes(x="",y=percent,fill=protein),stat="identity", width=1) +
  coord_polar("y", start=0)+
  facet_grid(~diet)+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  scale_fill_manual(values=c("#70AD47","#4A91D0","#FFC000","#C5E0B4"))

ggsave("figures/diets_trial1.png",width=5.99,height=4.25)  


# trial 2----
# remake pie charts
trial2<-data.frame(diet=rep(c("Menhaden Fish Oil",
                              "50/50 Diet","Invasive Carp Fish Oil"),2),
                   protein=c(rep("Menhaden Fish Oil (MFO)",3),
                             rep("Invasive Carp Fish Oil (ICFO)",3)),
                   percent=c(100,50,0,0,50,100))

trial2$diet<-factor(trial2$diet,c("Menhaden Fish Oil",
                                  "50/50 Diet","Invasive Carp Fish Oil"))
trial2$protein<-factor(trial2$protein,
                       levels=c("Menhaden Fish Oil (MFO)",
                                "Invasive Carp Fish Oil (ICFO)"))
ggplot(data=trial2)+
  geom_bar(aes(x="",y=percent,fill=protein),stat="identity", width=1) +
  coord_polar("y", start=0)+
  facet_grid(~diet)+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_fill_manual(values=c("#70AD47","#4A91D0"))

ggsave("figures/diets_trial2.png",width=5.99,height=4.25)  
