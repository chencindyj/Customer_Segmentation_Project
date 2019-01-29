require(data.table)
require(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(tidyr)
library(reshape2)

##PART I
#load file
setwd("U:/department/Admin&Finance/Measurements/Sales Operations/Cindy Chen/Project Bullseye")
idc <- as.data.frame(fread("RainKingbyDUNS.csv", header=T))
  idc <- na.omit(idc)
  idc$Primary.SIC.8.Digit <- sprintf("%08s", idc$Primary.SIC.8.Digit)
  idc <- as.data.table(idc)

#fix some things in your RainKing file so that you have IDCindustry
  siclib <-fread("DNB - SIC Code Library.csv", sep="auto", header=TRUE)
  siclib$SIC8 <- sprintf("%08d", siclib$SIC8) #change everything to characters so we can match it
  setnames(siclib, "SIC8", "Primary.SIC.8.Digit")
  setkey(idc, "Primary.SIC.8.Digit")
  idc <- merge(idc, unique(siclib[ , c("Primary.SIC.8.Digit", "IDCIndustry")]), by = "Primary.SIC.8.Digit", all.x=TRUE) #add in idc industry
  rm(siclib)  

  names(idc)[6] <- "it.budget.duns"
  
  #change the employee counts!
  idc[,All.Employees:=as.numeric(All.Employees)]
  idc[All.Employees > Emp.Here, Emp.Here := All.Employees]
  
  idc$Emp_Group[idc$Emp.Here >= 1 & idc$Emp.Here < 5] <- '0001-4'
  idc$Emp_Group[idc$Emp.Here >= 5 & idc$Emp.Here < 10] <- '0005-9'
  idc$Emp_Group[idc$Emp.Here >= 10 & idc$Emp.Here < 20] <- '0010-19'
  idc$Emp_Group[idc$Emp.Here >= 20 & idc$Emp.Here <50] <- '0020-49'
  idc$Emp_Group[idc$Emp.Here >= 50 & idc$Emp.Here <100] <- '0050-100'
  idc$Emp_Group[idc$Emp.Here >= 100 & idc$Emp.Here < 250] <- '0100-250'
  idc$Emp_Group[idc$Emp.Here >= 250 & idc$Emp.Here < 500] <- '0250-500'
  idc$Emp_Group[idc$Emp.Here >= 500 & idc$Emp.Here < 1000] <- '0500-1000'
  idc$Emp_Group[idc$Emp.Here >= 1000 & idc$Emp.Here < 3000] <- '1000-3000'
  idc$Emp_Group[idc$Emp.Here >= 3000] <- '3000+'
  
  idc <- subset(idc, Emp.Here >= 10)
  idc <- as.data.table(idc)
  idc[,it.budget.duns:=it.budget.duns*0.7] #to remove telecom spending from Rain King IT spends
  
  count.summary <- aggregate(Duns.Number ~ Emp_Group + IDCIndustry, data=idc, FUN=length)
  count.summary <- count.summary %>% spread(IDCIndustry, Duns.Number)
  write.csv(count.summary, "countsummary.csv", row.names = FALSE)
  
  idc[,total.spend.per.head:= it.budget.duns / Emp.Here]
  spendperhead <- aggregate(total.spend.per.head ~ Emp_Group + IDCIndustry + SIC2Des, data=idc, FUN=median)
  #spendperhead <- spendperhead %>% spread(IDCIndustry, total.spend.per.head) #dont make the csv in this table format though because it can't read it otherwise
  write.csv(spendperhead, "DUNS_Spendperhead.csv", row.names=FALSE)
  
  industry_sph <- aggregate(total.spend.per.head ~ Emp_Group + IDCIndustry, data=idc, FUN=median)
  write.csv(industry_sph, "spendperhead_DUNS_fillinmissing.csv", row.names=FALSE)
  
  rm(count.summary, total.it, spendperhead, idc, industry_sph)
  
#PART II
#LOAD THE WHOLE DATA SET
  setwd("U:/department/Admin&Finance/Measurements/Sales Operations/Cindy Chen/Project Bullseye")
  duns_data <- fread("RainKingbyDUNS.csv", header=T)
  duns_data <- duns_data[c(1:(nrow(duns_data)/2)),]
  duns_data <- duns_data[c((nrow(duns_data)/2+1):nrow(duns_data))] #the second half of the data
  
  duns_data <- duns_data[Emp.Here>0,]
  
  #fix some things in your RainKing file so that you have IDCindustry
  siclib <-fread("DNB - SIC Code Library.csv", sep="auto", header=TRUE)
  siclib$SIC2 <- sprintf("%02d", siclib$SIC2) #change everything to characters so we can match it
  setkey(duns_data, "SIC2")
  duns_data <- merge(duns_data, unique(siclib[ , c("SIC2", "IDCIndustry")]), by = "SIC2", all.x=TRUE) #add in idc industry
  rm(siclib)  
  
  names(duns_data)[6] <- "it.budget.duns"
  
  test <- read.csv("DUNS_Spendperhead.csv", header = T)
  test2 <- read.csv("spendperhead_DUNS_fillinmissing.csv", header=T)
  
  it.spend <- duns_data[,c("Duns.Number", "it.budget.duns", "Emp.Here", "IDCIndustry", "SIC2Des")]
  rm(duns_data)
  
  it.spend$Emp_Group[it.spend$Emp.Here >= 1 & it.spend$Emp.Here < 5] <- '0001-4'
  it.spend$Emp_Group[it.spend$Emp.Here >= 5 & it.spend$Emp.Here < 10] <- '0005-9'
  it.spend$Emp_Group[it.spend$Emp.Here >= 10 & it.spend$Emp.Here < 20] <- '0010-19'
  it.spend$Emp_Group[it.spend$Emp.Here >= 20 & it.spend$Emp.Here <50] <- '0020-49'
  it.spend$Emp_Group[it.spend$Emp.Here >= 50 & it.spend$Emp.Here <100] <- '0050-100'
  it.spend$Emp_Group[it.spend$Emp.Here >= 100 & it.spend$Emp.Here < 250] <- '0100-250'
  it.spend$Emp_Group[it.spend$Emp.Here >= 250 & it.spend$Emp.Here < 500] <- '0250-500'
  it.spend$Emp_Group[it.spend$Emp.Here >= 500 & it.spend$Emp.Here < 1000] <- '0500-1000'
  it.spend$Emp_Group[it.spend$Emp.Here >= 1000 & it.spend$Emp.Here < 3000] <- '1000-3000'
  it.spend$Emp_Group[it.spend$Emp.Here >= 3000] <- '3000+'
  
  it.spend <- it.spend[Emp.Here >= 10,]
  
  it.spend <- merge(it.spend, test2, by=c("IDCIndustry", "Emp_Group"), all.x=TRUE)
  it.spend <- merge(it.spend, test, by=c("SIC2Des", "Emp_Group", "IDCIndustry"), all.x=TRUE)
  it.spend[total.spend.per.head.y > 0,total.spend.per.head.x:=total.spend.per.head.y]
  
  it.spend[is.na(it.budget.duns), it.budget.duns:=Emp.Here * total.spend.per.head.x]
  
  it.spend <- as.data.table(it.spend)
  it.spend[is.na(it.spend$it.budget.duns), it.budget.duns:=0]
  it.spend[is.null(it.spend$Revenue..B.), Revenue..B.:=0]
  it.spend[is.na(it.spend$Revenue..B.), Revenue..B.:=0]
  it.spend[is.na(it.spend$revenue.millions), revenue.millions := 0]
  it.spend[is.null(it.spend$revenue.millions), revenue.millions := 0]
  it.spend[is.null(it.spend$All.Employees), All.Employees := 0]
  it.spend[is.na(it.spend$All.Employees), All.Employees := 0]
  
  it.spend <- it.spend[,-c("total.spend.per.head.y")]
  
  #write.csv(it.spend, "addressable_p1_DUNS.csv", row.names=FALSE)
  #write.csv(it.spend, "addressable_p2_DUNS.csv", row.names=FALSE)
  rm(test)