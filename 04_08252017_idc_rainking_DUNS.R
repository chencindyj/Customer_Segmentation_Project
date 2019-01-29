library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

alldata_idc <- as.data.frame(fread("RainKingbyDUNS.csv", header=T))
alldata_idc <- as.data.table(na.omit(alldata_idc))

siclib <-fread("DNB - SIC Code Library.csv", sep="auto", header=TRUE)
siclib$SIC8 <- sprintf("%08d", siclib$SIC8) #change everything to characters so we can match it
setnames(siclib, "SIC8", "Primary.SIC.8.Digit")
setkey(alldata_idc, "Primary.SIC.8.Digit")
alldata_idc <- merge(alldata_idc, unique(siclib[ , c("Primary.SIC.8.Digit", "IDCIndustry")]), by = "Primary.SIC.8.Digit", all.x=TRUE) #add in idc industry
rm(siclib)  

alldata_idc1 <- as.data.table(aggregate(revenue.millions ~ IDCIndustry + Duns.Number + Emp.Here, data=alldata_idc, FUN=sum))
spend_idc <- as.data.table(aggregate(IT.Budget..M. ~  IDCIndustry + Duns.Number, data=alldata_idc, FUN=sum))
            
IT.emp.idc <- merge(alldata_idc1, spend_idc, by=c("IDCIndustry", "Duns.Number"), nomatch=0)

IT.emp.idc[,spend.portion.rev:= ifelse(revenue.millions==0,0, IT.Budget..M./revenue.millions)]

IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 1 & IT.emp.idc$Emp.Here < 5] <- '0001-4'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 5 & IT.emp.idc$Emp.Here < 10] <- '0005-9'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 10 & IT.emp.idc$Emp.Here < 20] <- '0010-19'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 20 & IT.emp.idc$Emp.Here <50] <- '0020-49'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 50 & IT.emp.idc$Emp.Here <100] <- '0050-100'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 100 & IT.emp.idc$Emp.Here < 250] <- '0100-250'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 250 & IT.emp.idc$Emp.Here < 500] <- '0250-500'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 500 & IT.emp.idc$Emp.Here < 1000] <- '0500-1000'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 1000 & IT.emp.idc$Emp.Here < 3000] <- '1000-3000'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 3000] <- '3000+'
              
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here < 500] <- '00000-500'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 500 & IT.emp.idc$Emp.Here < 2500] <- '0500-2500'
IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 2500] <- '2500+'

#filter out outliers
IT.emp.idc <- IT.emp.idc[spend.portion.rev < 1,]

graph2 <- merge(aggregate(revenue.millions ~ IDCIndustry + Emp_Group, data=IT.emp.idc, FUN=sum),
                aggregate(IT.Budget..M. ~ IDCIndustry + Emp_Group, data=IT.emp.idc, FUN=sum),
                by=c("Emp_Group","IDCIndustry"), all.x=TRUE)
graph2$spend.portion.rev <- graph2$IT.Budget..M. / graph2$revenue.millions
graph2 <- as.data.table(graph2)
graph2 <- graph2[IDCIndustry !="19. Unclassified"]

################graph2 plot
portion_plot <- ggplot(graph2, aes(x=IDCIndustry, y=spend.portion.rev)) +
              geom_bar(aes(fill=Emp_Group), stat = "identity", position="dodge") +
              ggtitle("2015 Rain King IT Spend as % of Revenue") +
              xlab("Industries") +
              ylab("Percentage") +
              geom_hline(aes(yintercept = median(graph2$spend.portion.rev)),  colour = "red") +
              geom_text(aes( 3, median(graph2$spend.portion.rev), label = "Average IT Spend as % of Rev", vjust = -1), size = 3) + 
              scale_fill_discrete(name="Employee Size")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))
portion_plot
write.csv(graph2, "itspendasrevenue.csv", row.names=FALSE)
