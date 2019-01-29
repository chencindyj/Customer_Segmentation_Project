library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

alldata_idc <- as.data.frame(fread("mergeddata_06092017.csv", header=T))
            
            ###IT SPEND AS A % OF REVENUE###
          #if you want to find it by employee, REMOVE "IDC INDUSTRY" when you run the code!!

            alldata_idc1 <- as.data.table(aggregate(revenue.millions ~ IDCIndustry + Global.Ultimate.Duns.Number + Country + Global.Ultimate.Business.Name, data=alldata_idc, FUN=sum))
            alldata_idc1 <- subset(alldata_idc1, alldata_idc1$Country=="CA")
            
            spend_idc <- as.data.table(aggregate(it.budget.duns ~  IDCIndustry + Global.Ultimate.Duns.Number + Country + Global.Ultimate.Business.Name, data=alldata_idc, FUN=sum))
            spend_idc <- subset(spend_idc, spend_idc$Country=="CA")

            #filter out big companies
            #emp_count <- as.data.table(aggregate(Emp.Here ~  Global.Ultimate.Duns.Number + Country + Global.Ultimate.Business.Name, data=alldata_idc, FUN=sum))
            #emp_count <- subset(emp_count, emp_count$Country=="CA" & emp_count$Emp.Here < 10000  & emp_count$Emp.Here > 10)
                             
              #incorporate employee counts
                employees <- as.data.table(aggregate(Emp.Here ~ Global.Ultimate.Duns.Number + Country + Global.Ultimate.Business.Name, data=alldata_idc, FUN=sum))
                employees <- subset(employees, employees$Country=="CA")
            
            IT.emp.idc <- merge(alldata_idc1, spend_idc, by=c("IDCIndustry", "Global.Ultimate.Duns.Number", "Global.Ultimate.Business.Name"), nomatch=0)
            IT.emp.idc <- merge(IT.emp.idc,employees, by=c("Global.Ultimate.Duns.Number", "Global.Ultimate.Business.Name"), nomatch=0)
            #only include the next one if you're filtering out big companies
              #IT.emp.idc <- merge(IT.emp.idc, emp_count, by=c("Global.Ultimate.Duns.Number", "Global.Ultimate.Business.Name"), all.x=TRUE)
            
            IT.emp.idc[,spend.portion.rev:= ifelse(revenue.millions==0,0,it.budget.duns/revenue.millions)]
            IT.emp.idc <- na.omit(IT.emp.idc)
              
              IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here < 500] <- '00000-500'
              IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 500 & IT.emp.idc$Emp.Here < 2500] <- '0500-2500'
              IT.emp.idc$Emp_Group[IT.emp.idc$Emp.Here >= 2500] <- '2500+'
              
              #GRAPH1
              graph1 <- aggregate(spend.portion.rev ~ IDCIndustry + Emp_Group, data=IT.emp.idc, FUN=mean)
              graph1$spend.portion.rev <- graph1$spend.portion.rev*100
              #graph type: RainKing data
              
              #GRAPH2
              graph2 <- merge(aggregate(revenue.millions ~ IDCIndustry + Emp_Group, data=IT.emp.idc, FUN=sum),
                    aggregate(it.budget.duns ~ IDCIndustry + Emp_Group, data=IT.emp.idc, FUN=sum),
                    by=c("Emp_Group"), all.x=TRUE)
              graph2$spend.portion.rev <- graph2$it.budget.duns / graph2$revenue.millions
                
              ################graph1 plot
            portion_plot <- ggplot(graph1, aes(x=IDCIndustry, y=spend.portion.rev)) +
              geom_bar(aes(fill=Emp_Group), stat = "identity", position="dodge") +
              ggtitle("2015 Rain King Canada IT Spend as % of Revenue") +
              xlab("Industries") +
              ylab("Percentage") +
              geom_hline(aes(yintercept = mean(graph1$spend.portion.rev)),  colour = "red") +
              geom_text(aes( 3, mean(graph1$spend.portion.rev), label = "Average IT Spend as % of Rev", vjust = -1), size = 3) + 
              scale_y_continuous(breaks = 1:10) +
              scale_fill_discrete(name="Employee Size")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))
            portion_plot
            
################graph1 plot with only companies w/ 11-9999 employees
            portion_plot <- ggplot(graph1, aes(x=IDCIndustry, y=spend.portion.rev)) +
              geom_bar(aes(fill=Emp_Group), stat = "identity", position="dodge") +
              ggtitle("2015 Rain King Canada IT Spend as % of Revenue - Companies w 11-9999 employees") +
              xlab("Industries") +
              ylab("Percentage") +
              geom_hline(aes(yintercept = mean(graph1$spend.portion.rev)),  colour = "green") +
              geom_text(aes( 3, mean(graph1$spend.portion.rev), label = "Average IT Spend as % of Rev", vjust = -1), size = 3) +
              scale_fill_discrete(name="Employee Size")+
              scale_y_continuous(breaks = 1:10) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))
            portion_plot
            
            graph2 <- aggregate(it.budget.duns ~ Emp_Group, data=IT.emp.idc, FUN=sum)
            
            #i don't have IDC data to compare with this
            graph3 <- data.frame(matrix(c("01. Primary", 1509.9, "02. Discrete Manufacturing", 1817.6, "03. Process Manufacturing",
                                          2190.0,"04. Construction",652.4,"05. Transportation",1621.1,"06. Communications and Media",
                                          3829.4,"07. Utilities",1262.4,"08. Wholesale and Distribution",1262.7,"09. Retail",
                                          8136.1,"10. Banking",5684.3,"11. Insurance", 2220.5,"12. Financial Markets",1272.1,
                                          "13. Business Services",5904.5,"14. Government",6444.9,"15. Education",1308.9,"16. Health",1532.8),
                                        nrow = 16, ncol = 2, byrow = TRUE,
                                        dimnames = list(c(),c("IDCIndustry","it.budget.duns"))))
            graph3$it.budget.duns <- as.numeric(as.character(graph3$it.budget.duns)) #because they're previous factors
            sum(graph3$it.budget.duns)
            graph3$type <- "IDC"
            graph3$portion <- graph3$it.budget.duns / sum(graph3$it.budget.duns)
            combined <- rbind(graph3, graph1)
            
            portion_plot <- ggplot(combined, aes(x=IDCIndustry, y=portion, group = type, fill=type)) +
              geom_col(position = "dodge", width=.5) +
              ggtitle("2013 IDC Canada Portions vs 2015 Rain King Canada Portions") +
              xlab("Industries") +
              ylab("%") +
              scale_fill_discrete(guide = guide_legend(title = "Data Source")) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7))
            portion_plot
            