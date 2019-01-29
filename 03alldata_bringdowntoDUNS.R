library(data.table)
library(plyr)

  idc <- fread("Raw_Array.txt", sep = "auto", sep2="auto", header=TRUE)
  idc <- idc[,c(1:2,7:8)]
  rainking <- na.omit(fread("RainKingData__0331.csv", header=TRUE), cols="DUNS")
  siclib <-fread("DNB - SIC Code Library.csv", sep="auto", header=TRUE)
  
 names(idc) <- make.names(names(idc))

#prepare the rainking data for matching  
  rainking <- rainking[,c("Old.DUNS", "All Employees", "Revenue($B)", "IT Budget($M)")]   #filter out the columns that I don't want just so the left join is way easier
  names(rainking) <- make.names(names(rainking)) #change the headings
  rainking <- aggregate(cbind(IT.Budget..M., Revenue..B., All.Employees) ~ Old.DUNS, data=rainking, FUN=sum) #must do this step because DUNs are not unique
  rainking$Old.DUNS <- sprintf("%09d", rainking$Old.DUNS) #get everything in 9 digits
  rainking <- as.data.table(rainking)
  setnames(rainking, "Old.DUNS", "Duns.Number")  #rename DUNs Number column so that we can merge them shortly
  
  setkey(idc, "Duns.Number")
  setkey(rainking, "Duns.Number")
  idc <- merge(idc, rainking, all.x=TRUE) 
  rm(rainking)
  
  descriptions <- fread("Desc_Info.txt", sep="auto", sep2="auto", header=TRUE) #disable this line if you want all 19MM lines
  descriptions <- descriptions[,c(1:2,4)] #disable this line if you want all 19 MM
  names(descriptions) <- make.names(names(descriptions))
  idc <- merge(idc, descriptions, by=c("Duns.Number"), all.x=TRUE)
  rm(descriptions)

  #add SIC codes
  idc[,SIC4:= substr(Primary.SIC.8.Digit, 1, 4)]  #change SIC codes to SIC 4
  idc[,SIC2:= substr(Primary.SIC.8.Digit, 1, 2)] #change SIC codes to SIC 2
  
  #incorporate SIC code names
  siclib$SIC4 <- sprintf("%04d", siclib$SIC4) #change everything to characters so we can match it
  siclib$SIC2 <- sprintf("%02d", siclib$SIC2) #change everything to characters so we can match it
  siclib$SIC8 <- sprintf("%08d", siclib$SIC8) #change everything to characters so we can match it
  
  #i have to use unique() because siclib gives me a lot of entries for the same SIC2 code (since there are a lot of SIC8 entries),
    #so that i only have 1 SIC2 code to match 
  idc <- merge(idc, unique(siclib[ , c("SIC2", "SIC2Des")]), by = "SIC2", all.x=TRUE) #left join
  idc <- merge(idc, unique(siclib[ , c("SIC4", "SIC4Des")]), by = "SIC4", all.x=TRUE) #this is a left join. all the idc data is there and if rainking data match, it was appended

  setnames(siclib, "SIC8", "Primary.SIC.8.Digit")
  setkey(idc, "Primary.SIC.8.Digit")
  idc[, revenue.millions:=Revenue..B.*1000] #
  idc <- merge(idc, unique(siclib[ , c("Primary.SIC.8.Digit", "IDCIndustry")]), by = "Primary.SIC.8.Digit", all.x=TRUE) #add in idc industry

  rm(siclib, rainking)
  rm(descriptions)

  idc <- idc[order(-idc$Duns.Number),]
  
  write.csv(idc, "RainKingbyDUNS.csv", row.names=FALSE)
