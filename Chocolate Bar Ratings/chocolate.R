setwd("/home/ronlee/Documents/kaggle/Chocolate Bar Ratings")
library(tidyverse)
chocolate<-data.table::fread("flavors_of_cacao.csv")

glimpse(chocolate)
colnames(chocolate)<-c("Company","Specific_Bean_Origin","REF","Review_Date","Cocoa_percent",
                       "Company_location","Rating","Bean_Type","Broad_Bean_Region")
#feature engineering and feature formatting
chocolate$Factor_percent<-cut(chocolate$Rating,0:5)
chocolate$is_domestic<-if_else(chocolate$Broad_Bean_Region == chocolate$Company_location,"yes","no")
chocolate$Cocoa_percent<-as.numeric(str_sub(chocolate$Cocoa_percent,1,2))/100
sapply(chocolate,function(x){
  return(sum(is.na(x)))
})
#basic plot of some key features
chocolate %>% ggplot(aes(Rating))+geom_histogram(bins = 40)+ggthemes::theme_few()+
  ggtitle("Hist plot of the bars' rating")

chocolate %>% ggplot(aes(Cocoa_percent))+geom_histogram()+
  ggtitle("Hist plot of the cocoa percent")

#1.Where are the best cocoa beans grown?
#2.Which countries produce the highest-rated bars
chocolate %>% filter(Rating == 5) %>% DT::datatable()

#3.What’s the relationship between cocoa solids percentage and rating?
chocolate %>% ggplot(aes(x=Cocoa_percent,y=Rating))+geom_point()+
  stat_smooth(method = lm)

chocolate %>% ggplot(aes(x=Factor_percent,y=Cocoa_percent,fill=Factor_percent))+
  geom_boxplot()+ggthemes::theme_economist()+
  ggtitle("Relationship Between Cocoa Solids Percentage and Rating")

cor(chocolate$Cocoa_percent,chocolate$Rating)
#EDA
#1.check rating in every year
chocolate %>% group_by(as.factor(Review_Date)) %>% ggplot(aes(x=as.factor(Review_Date),y=Rating))+
  geom_boxplot(aes(fill=as.factor(Review_Date)))+theme(legend.position = "none")+labs(x="Review_Date",
                                                                                      title="Rating Changement In Every Year")
#2.which country contribute those high ranked chocolate bars 
chocolate %>% group_by(Company_location) %>% summarise(count=n()) %>% 
  ggplot(aes(x=Company_location,y=count,fill=Company_location))+geom_col()+theme(legend.position = "none")+
  coord_flip()+theme(axis.text.x = element_text(angle = 90))+ggtitle("Company numbers of different countries")

#3.what are differences of beans produced in domstic and not
table(chocolate$is_domestic)
chocolate %>% ggplot(aes(is_domestic)) + geom_bar(fill=c("red","blue"))+ggtitle("Bars Are Made Domestically???")
chocolate %>% ggplot(aes(is_domestic,Rating,fill=is_domestic))+geom_boxplot()+
  ggtitle("Rating Difference Between Domestically Made Bars And Not")

#4 goods flow from Broad_Bean_Region to Company_location
chocolate %>% group_by(Company_location,Broad_Bean_Region) %>% count() %>% filter(n>=5) %>%
  ggplot(aes(Company_location,Broad_Bean_Region,fill=n))+geom_tile()+scale_fill_distiller(palette = "Spectral")+
  labs(title="Goods Flow From Broad_Bean_Region To Company_location",fill="Frequence")+
  theme(axis.text.x = element_text(angle = 90))

chocolate %>% group_by(Company_location,Broad_Bean_Region) %>% summarise(rate=median(Rating)) %>% filter(rate>=2.5) %>%
  ggplot(aes(Company_location,Broad_Bean_Region,fill=rate))+geom_tile()+scale_fill_distiller(palette = "Spectral")+
  labs(title="Rating of Broad_Bean_Region To Company_location",fill="Rating")+
  theme(axis.text.x = element_text(angle = 90))