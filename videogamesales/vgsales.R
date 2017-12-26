setwd("E:/kaggle/videogamesales")
suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(stringr))
suppressMessages(library(DT))
vgame<-as.tibble(fread("vgsales.csv"))
str(vgame)
vgame<-vgame[!(vgame$Year %in% c("N/A","2020")),] %>% gather(Region, Revenue, 7:10)
vgame %>% na.omit() %>% ggplot(aes(Year))+geom_bar(aes())+labs(y="total count everyyear",title="sales count by year")+
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4), plot.title = element_text(size = 15, vjust = 2),axis.title.x = element_text(size = 12, vjust = -0.35))

vgame %>% group_by(Year) %>% summarise(re=sum(Global_Sales)) %>% ggplot(aes(x=Year,y=re))+
  geom_bar(stat = "identity",fill="maroon")+labs(y="Revenue",title="Revenue by Year")

top_sale_publisher<-vgame %>% mutate(Publisher=factor(Publisher)) %>% group_by(Year,Publisher) %>% summarise(n=sum(Global_Sales)) %>% top_n(1)
datatable(top_sale_publisher)
top_sale_publisher %>% ggplot(aes(x=Year,y=n,fill=Publisher))+geom_bar(stat = "identity")+labs(title="top publisher by year",y="Revenue")+
  theme(axis.title.x = element_text(angle = 90),plot.title = element_text(size=13),legend.position = "top")



