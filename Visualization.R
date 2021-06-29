install.packages("readxl")
library(readxl)

install.packages("ggplot2")
library(ggplot2)
SalesData = read.csv(choose.files())
View(SalesData)

install.packages("tidyr")
library(tidyr)
library(dplyr)

# 1. Compare sales by region for 2016 with 2015 using bar chart

s1 = SalesData%>%
  group_by(Region)%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))

data_long = gather(s1,key = Year, value = Sales,-Region )
data_long$Sales = round(data_long$Sales,1)

ggplot(data_long,aes(Region,Sales, fill = Year,label = Sales)) + geom_bar(stat = "identity",position = "dodge") + 
  geom_text(size = 4) + xlab('Region') +ylab('Sales') + ggtitle("Comparision of Sales by Region")


# 2. Pie charts for sales for each region in 2016 

install.packages("plotrix")
library(plotrix)

s2 = SalesData%>%
  group_by(Region)%>%
  summarise(TotalSales2016 = sum(Sales2016))

piepercent = round(s2$TotalSales2016/sum(s2$TotalSales2016)*100,1)
lbls= s2$Region
lbls = paste(lbls,":",piepercent)
lbls = paste(lbls,"%",sep = "")

pie(s2$TotalSales2016,labels = lbls , col = c("lightskyblue","royalblue3","turquoise4"),main = "2D Pie Chart of Sales 2016",radius = 1,border = "black")

pie3D(s2$TotalSales2016,labels = lbls ,explode = 0.1, col = c("lightskyblue","royalblue3","turquoise4")) + title('3D Pie Chart of Sales 2016')

# 3. Compare sales of 2015 and 2016 with region and tiers 

s3 = SalesData%>%
  group_by(Region,Tier)%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))

data_long1 = gather(s3,key = Year,value = Sales,-c(Region,Tier))
ggplot(data_long1,aes(Tier,Sales,fill = Year)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ Region) +ggtitle("Comparision of Sales by Region and Tiers")

# 4. In East Region, which state registered a decline in 2016 as compared to 2015?

s4 = SalesData%>%
  group_by(State)%>%
  filter(Region =="East")%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))

data_long2 = gather(s4,key = Year, value = Sales,-State)
ggplot(data_long2,aes(State,Sales,fill = Year)) + geom_bar(stat = "identity",position = "dodge") +ggtitle("Comparision of Sales by State")

# 5. In all the high tier, which division saw a decline in number of units sold in 2016 compared to 2015?

s5 = SalesData%>%
  filter(Tier == "High")%>%
  group_by(Division)%>%
  summarise(TotalUnit2015 = sum(Units2015),TotalUnit2016 = sum(Units2016))

data_long3 = gather(s5,key = Year, value = Sales,-Division)
gg1 = ggplot(data_long3,aes(Division,Sales, fill = Year)) + geom_bar(stat = "identity",position = "dodge")
gg1 + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Comparision of Sales by Division")

# 6. Create a new column qtr

SalesData$Qtr = if_else(SalesData$Month == "Jan"|SalesData$Month == "Feb"|SalesData$Month == "Mar","Q1",
                        if_else(SalesData$Month == "Apr"|SalesData$Month == "May"|SalesData$Month == "Jun","Q2",
                                if_else(SalesData$Month == "Jul"|SalesData$Month == "Aug"|SalesData$Month == "Sep","Q3","Q4")))

# 7. Compare Qtr wise sales in 2015 and 2016 in a bar plot

s7 = SalesData%>%
  group_by(Qtr)%>%
  summarise(TotalSum2015 = sum(Sales2015),TotalSum2016 = sum(Sales2016))

data_long4 = gather(s7,key = Year, value = Sales,-Qtr)
ggplot(data_long4,aes(Qtr,Sales,fill = Year)) + geom_bar(stat = "identity",position = "dodge") + ggtitle("Comparision of Sales by Quarter")

# 8. Determine the composition of Qtr wise sales in and 2015 with regards to all the tiers in a pie chart. 

s8_q1 = SalesData%>%group_by(Qtr,Tier)%>%filter(Qtr=="Q1")%>%summarise(TotalSales2015 = sum(Sales2015))
s8_q2 = SalesData%>%group_by(Qtr,Tier)%>%filter(Qtr=="Q2")%>%summarise(TotalSales2015 = sum(Sales2015))
s8_q3 = SalesData%>%group_by(Qtr,Tier)%>%filter(Qtr=="Q3")%>%summarise(TotalSales2015 = sum(Sales2015))
s8_q4 = SalesData%>%group_by(Qtr,Tier)%>%filter(Qtr=="Q4")%>%summarise(TotalSales2015 = sum(Sales2015))

piepercent1 = round(s8_q1$TotalSales2015/sum(s8_q1$TotalSales2015)*100,1)
piepercent2 = round(s8_q2$TotalSales2015/sum(s8_q2$TotalSales2015)*100,1)
piepercent3 = round(s8_q3$TotalSales2015/sum(s8_q3$TotalSales2015)*100,1)
piepercent4 = round(s8_q4$TotalSales2015/sum(s8_q4$TotalSales2015)*100,1)


lbls1 = s8_q1$Tier%>%paste(":",piepercent1)%>%paste("%",sep = "")         
lbls2 = s8_q2$Tier%>%paste(":",piepercent2)%>%paste("%",sep = "")         
lbls3 = s8_q3$Tier%>%paste(":",piepercent3)%>%paste("%",sep = "")         
lbls4 = s8_q4$Tier%>%paste(":",piepercent4)%>%paste("%",sep = "")         

par(mfrow = c(2,2)) 
pie(s8_q1$TotalSales2015,labels = lbls2 ,col = c("lightskyblue","royalblue3","turquoise4"),radius = 1,main = "Qtr 1")
pie(s8_q2$TotalSales2015,labels = lbls2 ,col = c("lightskyblue","royalblue3","turquoise4"),radius = 1,main = "Qtr 2")
pie(s8_q3$TotalSales2015,labels = lbls3 ,col = c("lightskyblue","royalblue3","turquoise4"),radius = 1,main = "Qtr 3")
pie(s8_q4$TotalSales2015,labels = lbls4 ,col = c("lightskyblue","royalblue3","turquoise4"),radius = 1,main = "Qtr 4")
