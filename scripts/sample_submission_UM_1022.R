#10/2022 Sample Submission to U of Missouri

#joiining together two excel files: UM Submission directory and Moisture Content Data
library(dplyr)
UM <- read.csv("odata/um_prox_data.csv")
moisture <- read.csv("odata/moisture.csv")
inner_join(x=UM, y=moisture, by="Sample")
sampledirectory <- inner_join(x=UM, y=moisture, by="Sample")

#exporting table
write.table(sampledirectory,file="UM_Sample_Submission_1022.csv", sep = ",")
