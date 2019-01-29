library(data.table)
library(plyr)

##RAW DATA TRANSFORMATION - incorporate Raw_Array, Desc_Info, RainKing, and SIC Library##
#run lines 6 and 7 if you want all the data
  descriptions <- fread("Desc_Info.txt", sep="auto", sep2="auto", header=TRUE) #disable this line if you want all 19MM lines
  descriptions <- descriptions[,c(1:2,4,6:7)] #disable this line if you want all 19 MM
  idc <- fread("Raw_Array.txt", sep = "auto", sep2="auto", header=TRUE)
  idc <- idc[,c(1:2,8:9)]
  rainking <- na.omit(fread("RainKingData__0331.csv", header=TRUE), cols="DUNS")
  siclib <-fread("DNB - SIC Code Library.csv", sep="auto", header=TRUE)

#left join for descriptions and idc data so that it appends descriptions to the DUNS numbers
 # setkey(idc, "Duns Number")
 # setkey(descriptions, "Duns Number")
 # idc1 <- merge(idc, descriptions, all.x=TRUE)
 # names(idc1) <- make.names(names(idc1))
 # rm(descriptions,idc)
  
 names(idc) <- make.names(names(idc))

#prepare the rainking data for matching  
  rainking <- rainking[,c("DUNS","Industry", "All Employees", "Revenue($B)", "IT Budget($M)")]   #filter out the columns that I don't want just so the left join is way easier
  names(rainking) <- make.names(names(rainking)) #change the headings
  rainking <- aggregate(cbind(IT.Budget..M., Revenue..B., All.Employees) ~ DUNS, data=rainking, FUN=sum) #aggregate!!! step i missed 05292017
  #as.data.frame.matrix(rainking)
  rainking$DUNS <- sprintf("%09d", rainking$DUNS) #get everything in 9 digits
  rainking <- as.data.table(rainking)
  setnames(rainking, "DUNS", "Global.Ultimate.Duns.Number")   #rename DUNs Number column so that we can merge them shortly
  
  #setkey(idc1, "Global.Ultimate.Duns.Number")
  setkey(idc, "Global.Ultimate.Duns.Number")
  setkey(rainking, "Global.Ultimate.Duns.Number")
  idc <- merge(idc, rainking, all.x=TRUE)
  #idc_fulldata <- merge(idc1, rainking, all.x=TRUE) #this is a left join. all the idc data is there and if rainking data match, it was appended
  #idc_fulldata <- data.table(as.data.frame(merge(idc1, rainking, nomatch=0))) #this is an inner join. i eliminated all rows with missing data
  
#now I want to divide out the GUNS spend and revenue, because in matching the GUNs data, each location is overstated (eg. if WalMart has an IT budget of $100Billion,
# I don't want each location to say it brings in $100B. I want to take $100B and divide by all the locations)
  
  idc$count <- 1 #so we can count the freq of each GUNS
  idc[, it.budget.duns:=max(IT.Budget..M.)/sum(count), by=c("Global.Ultimate.Duns.Number")] #this gets me the GUNS spend divided by the number of locations
  idc[, revenue.millions:=max(Revenue..B.)*1000/sum(count), by=c("Global.Ultimate.Duns.Number")] #repeat for revenue
  idc[, rk.employees:=max(All.Employees)/sum(count), by=c("Global.Ultimate.Duns.Number")] #repeat for revenue
  
  #idc_fulldata$count <- 1 #so we can count the freq of each GUNS
  #idc_fulldata[, it.budget.duns:=max(IT.Budget..M.)/sum(count), by=c("Global.Ultimate.Duns.Number")] #this gets me the GUNS spend divided by the number of locations
  #idc_fulldata[, revenue.millions:=max(Revenue..B.)*1000/sum(count), by=c("Global.Ultimate.Duns.Number")] #repeat for revenue
  #idc_fulldata[, rk.employees:=max(All.Employees)/sum(count), by=c("Global.Ultimate.Duns.Number")] #repeat for revenue

  #add SIC codes
  
  idc[,SIC4:= substr(Global.Primary.8.Digit.SIC.Cd, 1, 4)]  #change SIC codes to SIC 4
  idc[,SIC2:= substr(Global.Primary.8.Digit.SIC.Cd, 1, 2)] #change SIC codes to SIC 2
  
  #idc_fulldata[,SIC4:= substr(Global.Primary.8.Digit.SIC.Cd, 1, 4)]  #change SIC codes to SIC 4
  #idc_fulldata[,SIC2:= substr(Global.Primary.8.Digit.SIC.Cd, 1,2)] #change SIC codes to SIC 2
  
  #incorporate SIC code names
  siclib$SIC4 <- sprintf("%04d", siclib$SIC4) #change everything to characters so we can match it
  siclib$SIC2 <- sprintf("%02d", siclib$SIC2) #change everything to characters so we can match it
  siclib$SIC8 <- sprintf("%08d", siclib$SIC8) #change everything to characters so we can match it
  
  #i have to use unique() because siclib gives me a lot of entries for the same SIC2 code (since there are a lot of SIC8 entries),
    #so that i only have 1 SIC2 code to match 
  idc <- merge(idc, unique(siclib[ , c("SIC2", "SIC2Des")]), by = "SIC2", all.x=TRUE) #left join
  idc <- merge(idc, unique(siclib[ , c("SIC4", "SIC4Des")]), by = "SIC4", all.x=TRUE) #this is a left join. all the idc data is there and if rainking data match, it was appended

  setnames(siclib, "SIC8", "Global.Primary.8.Digit.SIC.Cd")
  setkey(idc, "Global.Primary.8.Digit.SIC.Cd")
  idc <- idc[,-c("IT.Budget..M.", "Revenue..B.", "All.Employees")]
  idc <- merge(idc, unique(siclib[ , c("Global.Primary.8.Digit.SIC.Cd", "IDCIndustry")]), by = "Global.Primary.8.Digit.SIC.Cd", all.x=TRUE) #add in idc industry
  
  #idc_fulldata <- merge(idc_fulldata, unique(siclib[ , c("SIC2", "SIC2Des")]), by = "SIC2", all.x=TRUE) #left join
  #idc_fulldata <- merge(idc_fulldata, unique(siclib[ , c("SIC4", "SIC4Des")]), by = "SIC4", all.x=TRUE) #this is a left join. all the idc data is there and if rainking data match, it was appended
  #idc_fulldata <- merge(idc_fulldata, unique(siclib[ , c("SIC8", "IDCIndustry")]), by = "SIC8", all.x=TRUE) #add in idc industry
  #setnames(siclib, "SIC8", "Global.Primary.8.Digit.SIC.Cd")
  #setkey(idc_fulldata, "Global.Primary.8.Digit.SIC.Cd")
  #idc_fulldata <- merge(idc_fulldata, unique(siclib[ , c("Global.Primary.8.Digit.SIC.Cd","SIC8Des")]), by = "Global.Primary.8.Digit.SIC.Cd", all.x=TRUE) #left join

  #clean up column order in idc_fulldata
  
  #idc_fulldata <- idc_fulldata[,c(1:8,10:20)]
  #write.csv(idc_fulldata, "mergeddata_06092017.csv", row.names=FALSE)

  write.csv(idc, "mergeddata_06092017.csv", row.names=FALSE)
  
  idc[is.na(idc$it.budget.duns), it.budget.duns:=0]
  idc[is.na(idc$revenue.millions), revenue.millions:=0]
  idc[is.na(idc$rk.employees), rk.employees:=0]
  write.csv(idc, "mergeddata_06092017_zeros.csv", row.names=FALSE)
rm(siclib, rainking)
rm(idc1, idc, idc_fulldata)
