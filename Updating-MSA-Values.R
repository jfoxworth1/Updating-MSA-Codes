# ```` updating MSA Values
#WorkingDir <- "/home/me/Documents/Uni-Jena/Work/HIWI/HIWI-2017-10-19-Updating-MSA-Codes"
#setwd(WorkingDir)
library(tidyverse)
library(readxl)
library(openxlsx)
WorkingDir <- readline(prompt = "Please enter the file path to where the files are located: ")
setwd(WorkingDir)

#Prompts
print("Please download the csv file from this website:")
print("https://www.bea.gov/regional/docs/msalist.cfm")
filePath.new <- readline(prompt = "Please enter the name of the new MSA csv file: ")
MSA <- read_csv(filePath.new, col_names = FALSE)
rm(filePath.new)

filePath.mixed <- readline(prompt = "Please enter the name of the mixed MSA XLSX file: ")
WhichSheet.mixed <- readline(prompt = "Which worksheet contains the mixed MSA information? ")
XWALK <- read_xlsx(filePath.mixed, sheet = WhichSheet.mixed)

rm(WhichSheet.mixed)
rm(WhichSheet.mixed)
# Old hard coded filepaths
#load files into memory
#MSA <- read_csv("BEA-MSA.csv", col_names = FALSE)
#CROSSWALK <- read_xlsx("MSA-FIPS-Crosswalk.xlsx", sheet = "CROSSWALK")
#XWALK <- read_xlsx("MSA-FIPS-Crosswalk.xlsx", sheet ="XWALK")

# Make new file to merge everything into
tmp <- 1:nrow(MSA[,1])
MSA <- cbind(tmp, MSA)
colnames(MSA) <-  c("Total#", "St FIPS", "State", "New MSA FIPS", "New MSA Name")

tmp <- mean(nchar(MSA$`St FIPS`))
MSA$`St FIPS` <- as.numeric(MSA$`St FIPS`)
MSA$`St FIPS` <- MSA$`St FIPS`/10^(tmp-2)
MSA$`New MSA FIPS` <- as.numeric(MSA$`New MSA FIPS`)

tmp <- rep_len(NA, length(MSA$`Total#`))
MSA <- cbind(MSA, tmp)
MSA <- MSA[, c("Total#", "tmp", "St FIPS", "State", "New MSA FIPS", "New MSA Name")]
colnames(MSA)[names(MSA) == 'tmp'] <- "#per ST"

# Number per state
tmp <- MSA$`St FIPS` %>%
    max()
for (i in 1:tmp){
  counter <- 0
  for(j in 1:length(MSA$`St FIPS`)){
    if(MSA$`St FIPS`[j] == i){
      counter <- counter + 1
      MSA$`#per ST`[j] <- counter
    }
  }
}
rm(counter)
# XWALK 
# Only contain existing MSA
XWALK.edit <- XWALK
colnames(XWALK.edit)[5] <- "New MSA FIPS"
XWALK.edit <- XWALK.edit[!is.na(XWALK.edit$`New MSA FIPS`),]

#remove duplicate MSA FIPS with 2 digit values
is.rural <- rep(FALSE, length(XWALK.edit$`New MSA FIPS`))
XWALK.edit <- cbind(XWALK.edit, is.rural)
XWALK.edit$is.rural[nchar(as.numeric(str_split(XWALK.edit$`Old MSA`, " ", simplify = TRUE)[,1]))<=2] <- TRUE

for(i in 1:length(XWALK.edit$`New MSA FIPS`)){ 
  if(XWALK.edit$`New MSA FIPS`[i] %in% XWALK.edit$`New MSA FIPS`[-i] && XWALK.edit$is.rural[i]){
   XWALK.edit <- XWALK.edit[-i,]
  }
}
XWALK.edit <-  XWALK.edit[!duplicated(XWALK.edit$`New MSA FIPS`),]
XWALK.edit$is.rural <- NULL
rm(is.rural)
rm(tmp)
# Combine with MSA
MSA.combine <- data.frame("Old MSA FIPS" = rep(NA, 439), "Old MSA name" = rep(NA, 439), "Comments" = rep(NA, 439))
MSA <- cbind(MSA, MSA.combine)
rm(MSA.combine)

#Combine XWALK into MSA
for(i in 1:439){
  for(j in 1:length(XWALK.edit$`New MSA FIPS`)){
    if(MSA$`New MSA FIPS`[i] == XWALK.edit$`New MSA FIPS`[j]){
      MSA$Old.MSA.FIPS[i] <- XWALK.edit$`Old MSA`[j]
      MSA$Old.MSA.name[i] <- XWALK.edit$`OldMSA Name`[j]
    }
  }
}

# Add comments
for(i in 1:439){
  if(is.na(MSA$Old.MSA.FIPS[i])){
    MSA$Comments[i] <- "Not in XWALK file"
  }
  else{
    if(nchar(MSA$Old.MSA.FIPS[i])<3){
      MSA$Comments[i] <- paste(as.numeric(str_split(MSA$Old.MSA.FIPS[i], " ", simplify = TRUE)[1,1]), "code seems to indicate that in the old coding it was rural")
    }
  }  
}
rm(i)
rm(j)
rm(WorkingDir)
write.xlsx(MSA, "MSA-FIPS-CROSSWALK-New.xlsx")
print("New file saved as MSA-FIPS-CROSSWALK-New.xlsx")
print("Done")