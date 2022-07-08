# This script can be used to randomly assign treatments to tanks

# written by S.K. Archer 3/28/22

# for both trails we have 9 tanks and 3 treatments
# install the package we'll use
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

# First create a vector of tank IDs, in this case just 1-9

tanks<-seq(1,9,1)

# then create a vector of treatments IDs, each treatment should be repeated 3 times, 
# because it will be assigned to three tanks

treatID.trial1<-rep(c("Control","ACFMforMFM","All.ACFM"),3)
treatID.trial2<-rep(c("Control","ACFO50.MFO50","ACFO100"),3)

# now we can create a data frame that has treatments randomly assigned to tanks

tank.assignments<-data.frame(tank=sample(tanks,9,replace=FALSE),
                             trial1=sample(treatID.trial1,9,replace=FALSE),
                             trial2=sample(treatID.trial2,9,replace=FALSE))%>%
  arrange(tank)

# now link to diet numbers for ease of reference  

diet1<-data.frame(trial1=c("Control","ACFMforMFM","All.ACFM"),
                  diet.trial1=c(1,2,3))
diet2<-data.frame(trial2=c("Control","ACFO50.MFO50","ACFO100"),
                  diet.trial2=c(4,5,6))

tank.assignments<-left_join(tank.assignments,diet1)
tank.assignments<-left_join(tank.assignments,diet2)

# now we can save this for our records
write.csv(tank.assignments,"wdata/treatment_assignements.csv")



