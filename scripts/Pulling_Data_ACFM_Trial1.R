# pulling/downloading data sheets from ACFM Feeding Trial 1


#downloading gsheet package
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(car)){install.packages("car")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(lmerTest)){install.packages("lmerTest")}

# pull the samplingcalculations.R data from ACFM Tial 1 google drive

saurl <- "https://docs.google.com/spreadsheets/d/1pPBfcCelEU_XjhyCIuJY7qCNYh-ufLbA/edit#gid=1011687095"

sturl <- "https://docs.google.com/spreadsheets/d/1pPBfcCelEU_XjhyCIuJY7qCNYh-ufLbA/edit#gid=1549175690"

samp <- read.csv(text=gsheet2text(saurl,format="csv"),stringsAsFactors = F)

stock <- read.csv(text=gsheet2text(sturl,format="csv"),stringsAsFactors = F)

#making linear model for HSI in samplingcalculations

HSI.lm <- lm(HSI~diet.name,samp)
plot(HSI.lm)
