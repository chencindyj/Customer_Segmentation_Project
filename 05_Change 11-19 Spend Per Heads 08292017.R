require(data.table)
require(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(tidyr)
library(reshape2)

  it.spend.1 <- read.csv("addressable_p2_DUNS.csv", header=T)
  it.spend <- read.csv("addressable_p1_DUNS.csv", header=T)
  it.spend <- as.data.table(rbind(it.spend.1, it.spend))
  rm(it.spend.1)
  
  it.spend[is.na(Revenue..B.),Revenue..B.:=0]
  it.spend[is.na(revenue.millions),revenue.millions:=0]
  it.spend[is.na(All.Employees),All.Employees:=0]
  
  softchoice <- as.data.table(read.csv("DunsNoGuns.SalesParents.2016.csv", header=T))
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} #change factors into numeric
  softchoice$Softchoice.Rev <- as.numeric.factor(softchoice$Softchoice.Rev)
  softchoice$Softchoice.Virt.GP <- as.numeric.factor(softchoice$Softchoice.Virt.GP)
  
  softchoice$Duns.Number <- as.character(softchoice$Duns.Number)
  softchoice$Duns.Number <- sprintf("%09s", softchoice$Duns.Number)
  softchoice$Duns.Number <- gsub(" ", "0", softchoice$Duns.Number)
  it.spend$Duns.Number <- sprintf("%09s", it.spend$Duns.Number)
  it.spend <- merge(it.spend, softchoice, by=c("Duns.Number"), all.x=TRUE)
  current_customers <- na.omit(it.spend) #TO MAKE A SEPARATE TABLE
  it.spend.softchoice <- aggregate(cbind(Softchoice.Rev, Softchoice.GP) ~ Emp_Group + SIC2Des, data=it.spend, FUN=sum) #softchoice.GP is in USD
  rm(softchoice)
  
  #Create tables summarizing by SIC2Des and Employee Group
  it.spend.heads <- aggregate(Emp.Here ~ SIC2Des + IDCIndustry + Emp_Group, data=it.spend, FUN=median)  
  it.spend.companies <- aggregate(Duns.Number ~ SIC2Des + IDCIndustry + Emp_Group, data=it.spend, FUN=length)
  
  #incorporate Total Spend Per Head
  spendperhead <- as.data.table(read.csv("DUNS_spendperhead.csv", header=T))
  spendperhead <- spendperhead[spendperhead$Emp_Group!="00010-19"] #delete all 11-19
  temp <- aggregate(total.spend.per.head ~ SIC2Des, spendperhead, FUN=mean)
  spendperhead <- as.data.table(read.csv("DUNS_spendperhead.csv", header=T))
  spendperhead <- merge(spendperhead, temp, by=c("SIC2Des"), all.x=TRUE)
  spendperhead[Emp_Group == "00010-19",total.spend.per.head.x:= total.spend.per.head.y]
  spendperhead <- spendperhead[,c(1:4)]
  names(spendperhead)[4] <- "total.spend.per.head"
  it.spend.heads <- merge(it.spend.heads, spendperhead, by=c("Emp_Group", "IDCIndustry", "SIC2Des"), all.x=TRUE)
  
  #fill in missing values; these are the median spends by industry (not SIC2Des)
  fillin <- read.csv("spendperhead_DUNS_fillinmissing.csv", header=T)
  names(fillin)[3] <- "generic.spend.per.head"
  it.spend.heads <- as.data.table(it.spend.heads)
  it.spend.heads <- merge(it.spend.heads, fillin, by=c("Emp_Group", "IDCIndustry"), all.x=TRUE)
  it.spend.heads[is.na(it.spend.heads$total.spend.per.head), total.spend.per.head:= generic.spend.per.head]
  rm(fillin)
  it.spend.heads <- na.omit(it.spend.heads)[,-c("generic.spend.per.head")]
  
  #Step 5: Multiply all IT spends by addressable.portions
  addressable.portions <- read.csv("AddressableWalletPortions.csv", header=T)
  
  #to match the addressable wallet table
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 0 & it.spend.heads$Emp.Here <= 5] <- 'x1.4'
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 5 & it.spend.heads$Emp.Here <= 9] <- 'x5.9'
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 10 & it.spend.heads$Emp.Here <= 19] <- 'x10.19'
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 20 & it.spend.heads$Emp.Here <= 49] <- 'x20.49'
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 50 & it.spend.heads$Emp.Here <= 99] <- 'x50.99'
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 100 & it.spend.heads$Emp.Here <= 199] <- 'x100.199'
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 200 & it.spend.heads$Emp.Here <= 499] <- 'x200.499'
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 500 & it.spend.heads$Emp.Here <= 999] <- 'x500.999'
  it.spend.heads$Addressable[it.spend.heads$Emp.Here >= 1000] <- 'x1000.'
  
  #melt the table so we can process it
  addressable.portions <- melt(addressable.portions)
  colnames(addressable.portions) <- c("IDCIndustry","Addressable","portion")
  
  it.spend.heads <- merge(it.spend.heads, addressable.portions, by=c("IDCIndustry","Addressable"), all.x=TRUE)
  it.spend.heads[,addressable.sph:= total.spend.per.head * portion]
  
  names(it.spend.companies)[4] <- "Count.of.Companies"
  it.spend.heads <- merge(it.spend.heads, it.spend.companies, by=c("SIC2Des", "Emp_Group", "IDCIndustry"), all.x=TRUE)
  rm(it.spend.companies)

  #create addressable spend by SIC2Des and Emp_Group  
    it.spend.heads[,addressable.spend:=addressable.sph*Emp.Here*Count.of.Companies/1000] #tested this, and the sum comes up to $659B for all; $257B for 50+ employees
      #create a company spend
        it.spend.heads[,add.per.company:=addressable.sph*Emp.Here]
        
        #incorporate in the complexity score
        high <- melt(read.csv("highcomplexity.csv", header=T))
        medium <- melt(read.csv("mediumcomplexity.csv", header=T))
        low <- melt(read.csv("lowcomplexity.csv", header=T))
        
        names(high) <- c("IDCIndustry", "Addressable", "h")
        names(medium) <- c("IDCIndustry", "Addressable", "m")
        names(low) <-  c("IDCIndustry", "Addressable", "l")
  
        it.spend.heads <- merge(it.spend.heads, high, by=c("IDCIndustry", "Addressable"), all.x=TRUE)
        it.spend.heads <- merge(it.spend.heads, medium, by=c("IDCIndustry", "Addressable"), all.x=TRUE)
        it.spend.heads <- merge(it.spend.heads, low, by=c("IDCIndustry", "Addressable"), all.x=TRUE)
        it.spend.heads <- na.omit(it.spend.heads)
        rm(high, low, medium)
        
        it.spend.heads[,complexity.score:=8*h + 1.5*m + l*0.5] #chose units of 8, 3, and 0.5
        
        #incorporate softchoice wallet
        it.spend.heads <- merge(it.spend.heads, it.spend.softchoice, by=c("SIC2Des", "Emp_Group"), all.x=TRUE)
        it.spend.heads[is.na(Softchoice.Rev),Softchoice.Rev:=0]
        it.spend.heads[is.na(Softchoice.GP), Softchoice.GP:=0]
        
        it.spend.heads[,Softchoice.Rev:=Softchoice.Rev/1000000000]
        it.spend.heads[,Softchoice.GP:=Softchoice.GP/1000000000]
        
        it.spend.heads[,softchoice.share:=Softchoice.Rev/addressable.spend]
        rm(it.spend.softchoice, it.spend)
        
        write.csv(it.spend.heads,"DUNS_spendestimate.csv", row.names=FALSE)
        