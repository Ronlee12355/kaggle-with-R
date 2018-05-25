setwd("E:/kaggle/io")
library(plotly,quietly = T)
library(data.table,quietly = T)
library(tidyverse,quietly = T)
library(lubridate)
library(stringr,quietly = T)
library(treemapify)
library(tm)
library(magrittr)
library(tidytext)

Teacher<-fread("Teachers.csv")
Donations<-fread("Donations.csv")
Donors<-fread("Donors.csv")
Projects<-fread("Projects.csv")
Resources<-fread("Resources.csv")
Schools<-fread("Schools.csv")

mean(Donations$`Donation Amount`);median(Donations$`Donation Amount`);max(Donations$`Donation Amount`);min(Donations$`Donation Amount`)
#平均值60.669，中位数值25，最大60000USD，最少0.01USD
p1<-Donations %>% filter(`Donation Amount`<=median(Donations$`Donation Amount`)) %>% ggplot(aes(x=`Donation Amount`))+geom_histogram(fill="red")+
  labs(y="Total Counts",title="Amount distribution")+theme_bw()
p2<-Donations %>% ggplot(aes(x=`Donation Included Optional Donation`))+geom_bar(fill=c("red","blue"))+coord_polar(theta = "x")+
  theme(axis.text.y = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())+
  labs(title="Donation Included Optional Donation")
p3<-Donors %>% filter(!is.na(`Donor Is Teacher`)) %>% group_by(`Donor Is Teacher`) %>% summarise(Total = n()) %>%
  arrange(desc(Total)) %>% ungroup() %>% ggplot(aes(x=`Donor Is Teacher`))+geom_bar(fill=c("red","blue"))+coord_polar(theta = "x")+
  theme(axis.text.y = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())+
  labs(title="Donor Is Teacher")
grid.arrange(p1,p2,p3,ncol=1)
#主要接受日期时间分布
Donations<-Donations %>% rename(DRD=`Donation Received Date`) %>% mutate(DRD=ymd_hms(DRD))
Donations %>% mutate(yyy=year(DRD)) %>% ggplot(aes(x=yyy,fill="#b388ff"))+
  geom_density(aes(y=..scaled..))+labs(y="",x="",title="Donation Received Date Count")+ggthemes::theme_pander()+
  theme(legend.position = "none")

p1<-Donors %>% filter(`Donor City`!='' & !is.na(`Donor City`)) %>% group_by(`Donor City`) %>% summarise(total=n()) %>% arrange(desc(total)) %>%
  top_n(15,wt=total) %>% ggplot(aes(x=`Donor City`,y=total,fill=as.factor(`Donor City`)))+geom_bar(stat="identity")+coord_flip()+
  labs(title="Donors City Frequency",fill="",y="",x="")+theme(legend.position = "none")
p2<-Donors %>% filter(`Donor State`!='' & !is.na(`Donor State`)) %>% group_by(`Donor State`) %>% summarise(total=n()) %>% arrange(desc(total)) %>%
  top_n(15,wt=total) %>% mutate(`Donor State`=reorder(`Donor State`,total)) %>% ggplot(aes(x=`Donor State`,y=total,fill=as.factor(`Donor State`)))+geom_bar(stat="identity")+coord_flip()+
  labs(title="Donor State Frequency",fill="",y="",x="")+theme(legend.position = "none")
grid.arrange(p1,p2)

Projects %>% filter(`Project Current Status`!='' && !is.na(`Project Current Status`)) %>% group_by(`Project Current Status`) %>%
  summarise(total=n()) %>% mutate(`Project Current Status`=as.factor(`Project Current Status`)) %>% 
  plot_ly(labels=~`Project Current Status`,values=~total,type="pie",textposition = 'inside',textinfo = 'label+percent') #%>% 
  #layout(title="Project Current Status Calculation")
Projects %>% filter(`Project Resource Category`!='' && !is.na(`Project Resource Category`)) %>% group_by(`Project Resource Category`) %>%
  summarise(total=n()) %>% mutate(`Project Resource Category`=as.factor(`Project Resource Category`)) %>% 
  plot_ly(labels=~`Project Resource Category`,values=~total,type="pie",textposition = 'inside',textinfo = 'label+percent') #%>% 
 # layout(title="Project Resource Category Calculation")
p3<-Projects %>% filter(`Project Type`!='' && !is.na(`Project Type`)) %>% group_by(`Project Type`) %>%
  summarise(total=n()) %>% mutate(`Project Type`=as.factor(`Project Type`)) %>% ggplot(aes(x=`Project Type`,y=total,fill=as.factor(`Project Type`)))+
  geom_bar(stat = "identity")+labs(title="Project Type",y="",x="",fill="")+theme(legend.position = "none")+coord_flip()
p4<-Projects %>% filter(`Project Resource Category`!='' && !is.na(`Project Resource Category`)) %>% group_by(`Project Resource Category`) %>%
  summarise(total=n()) %>% mutate(`Project Resource Category`=as.factor(`Project Resource Category`)) %>% ggplot(aes(x=`Project Resource Category`,y=total,fill=as.factor(`Project Resource Category`)))+
  geom_bar(stat = "identity")+labs(title="Project Resource Category",y="",x="",fill="")+theme(legend.position = "none",axis.text.x = element_blank())+coord_flip()
grid.arrange(p3,p4,nrow=2)

p1<-Projects %>% filter(`Project Subject Category Tree`!='' && !is.na(`Project Subject Category Tree`)) %>% group_by(`Project Subject Category Tree`) %>%
  summarise(total=n()) %>% filter(total>20000) %>% ggplot(aes(area=total,fill=total,label=as.factor(`Project Subject Category Tree`)))+
  geom_treemap()+geom_treemap_text(fontface = "italic", colour = "red", place = "centre",grow = TRUE,alpha=1)+
  scale_fill_continuous(low="lightblue",high = "darkblue")+ggtitle("Project Subject Category Tree")
p2<-Projects %>% filter(`Project Subject Subcategory Tree`!='' && !is.na(`Project Subject Subcategory Tree`)) %>% group_by(`Project Subject Subcategory Tree`) %>%
  summarise(total=n()) %>% filter(total>20000) %>% ggplot(aes(area=total,fill=total,label=as.factor(`Project Subject Subcategory Tree`)))+
  geom_treemap()+geom_treemap_text(fontface = "italic", colour = "blue", place = "centre",grow = TRUE,alpha=1)+
  scale_fill_continuous(low="white",high = "red")+ggtitle("Project Subject Subcategory Tree")
grid.arrange(p1,p2,nrow=1)
Projects %>% sample_n(200) %>% unnest_tokens(word,`Project Essay`) %>% filter(!(word %in% stop_words$word)) %>%
  count(word,sort = TRUE) %>% ungroup() %>% top_n(200,wt=word) %>% filter(str_detect(word,letters)) %>%ggplot(aes(word, n)) +
  geom_col(fill = "#ffc107") + labs(x = NULL, y = "tf-idf") + coord_flip() + theme_bw()+ggtitle("Words in Project Essay")

p1<-Projects %>% group_by(`Project Posted Date`) %>% summarise(total=n()) %>% mutate(`Project Posted Date`=as.POSIXct(`Project Posted Date`)) %>% ggplot(aes(x=`Project Posted Date`,y=total,group=1))+
  geom_path(color="blue")+scale_x_datetime(limits = as.POSIXct(c("2013-01-01","2018-05-01")))+labs(y="",title="Project Posted Date")
p2<-Projects %>% group_by(`Project Expiration Date`) %>% summarise(total=n()) %>% filter(`Project Expiration Date` != "") %>% mutate(`Project Expiration Date`=as.POSIXct(`Project Expiration Date`)) %>% ggplot(aes(x=`Project Expiration Date`,y=total,group=1))+
  geom_path(color="red")+scale_x_datetime(limits = as.POSIXct(c("2013-05-01","2018-12-31")))+labs(y="",title="Project Expiration Date")
p3<-Projects %>% group_by(`Project Fully Funded Date`) %>% summarise(total=n()) %>% filter(`Project Fully Funded Date` != "") %>% mutate(`Project Fully Funded Date`=as.POSIXct(`Project Fully Funded Date`)) %>% ggplot(aes(x=`Project Fully Funded Date`,y=total,group=1))+
  geom_path(color="green")+scale_x_datetime(limits = as.POSIXct(c("2013-05-01","2018-12-31")))+labs(y="",title="Project Fully Funded Date")
gridExtra::grid.arrange(p1,p2,p3,nrow=3)
Projects %>% group_by(`Project Type`) %>% summarise(total=n()) %>% arrange(desc(total)) %>% knitr::kable()
Projects %>% unnest_tokens(word,`Project Title`) %>% filter(!(word %in% stop_words$word)) %>%
  count(word,sort = TRUE) %>% ungroup()  %>% head(100) %>%
  with(wordcloud::wordcloud(word, n, max.words = 30,colors=RColorBrewer::brewer.pal(8, "Dark2")))

#school
p1<-Schools %>% filter(`School Name` != "" && !is.na(`School Name`)) %>% group_by(`School Name`) %>% summarise(total=n()) %>%
  arrange(desc(total)) %>% top_n(20,wt=total) %>% ggplot(aes(x=`School Name`,y=total,fill=as.factor(`School Name`)))+
  geom_bar(stat = "identity")+labs(y="",fill="",x="",title="School Name")+coord_flip()+theme(legend.position = "none")
p2<-Schools %>% filter(`School City` != "" && !is.na(`School City`)) %>% group_by(`School City`) %>% summarise(total=n()) %>%
  arrange(desc(total)) %>% top_n(20,wt=total) %>% ggplot(aes(x=`School City`,y=total,fill=as.factor(`School City`)))+
  geom_bar(stat = "identity")+labs(y="",fill="",x="",title="School City")+coord_flip()+theme(legend.position = "none")
p3<-Schools %>% filter(`School State` != "" && !is.na(`School State`)) %>% group_by(`School State`) %>% summarise(total=n()) %>%
  arrange(desc(total)) %>% top_n(20,wt=total) %>% ggplot(aes(x=reorder(`School State`,total),y=total,fill=as.factor(`School State`)))+
  geom_bar(stat = "identity")+labs(y="",fill="",x="",title="School State")+coord_flip()+ggthemes::theme_economist()+
  theme(legend.position = "none")
p4<-Schools %>% filter(`School District` != "" && !is.na(`School District`)) %>% group_by(`School District`) %>% summarise(total=n()) %>%
  arrange(desc(total)) %>% top_n(20,wt=total) %>% ggplot(aes(x=reorder(`School District`,total),y=total,fill=as.factor(`School District`)))+
  geom_bar(stat = "identity")+labs(y="",fill="",x="",title="School District")+coord_flip()+ggthemes::theme_tufte()+
  theme(legend.position = "none")
gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

p1<-Schools %>%
  rename(FreeLunch = `School Percentage Free Lunch`) %>%
  ggplot(aes(x = FreeLunch)) +
  geom_histogram(bins = 30,fill = "orange") +
  labs(x= 'School Percentage Free Lunch',y = 'Count', title = paste("Distribution of", ' School Percentage Free Lunch ')) +
  theme_bw()
range(Resources$`Resource Unit Price` %>% na.omit())
p2<-Resources %>% rename(uprice=`Resource Unit Price`) %>% na.omit() %>% filter(uprice<=1000) %>%ggplot(aes(x=uprice)) + geom_density(fill="#558b2f")+
  labs(x="Resource Unit Price",title="Distribution of Resource Unit Price",y="")+
  ggplot2::annotate("text",x=800,y=0.04,label="range of price 0~97085.5")+theme_bw()
Projects$duration <- as.numeric(as.Date(Projects$`Project Fully Funded Date`)-as.Date(Projects$`Project Posted Date`))
p3<-Projects %>% na.omit() %>% ggplot(aes(duration))+geom_density(fill="#0277bd")+labs(y="",title="Duration of Time duration")

Resources %>% sample_n(5e3) %>% rename(rin=`Resource Item Name`) %>% filter(rin != '' && !is.na(rin)) %>%
  unnest_tokens(word,rin) %>% filter(!(word %in% stop_words$word)) %>% count(word,sort=T)  %>% top_n(30,wt=n) %>%
  filter(str_length(word) >= 2 & !(word %in% as.character(c(1:1000)))) %>% ggplot(aes(x=word,y=n,fill=as.factor(word)))+geom_bar(stat = "identity")+
  coord_polar(theta = "x")+labs(title="Resource Item Name Frequency",y="",x="")+ggthemes::theme_fivethirtyeight()+
  theme(legend.position = "none",axis.text.y = element_blank())

teacher_amount<-inner_join(Donations,Donors,by="Donor ID")
teacher_amount %>% group_by(`Donor Is Teacher`,`Donation Included Optional Donation`) %>% summarise(total=n()) %>%
  ggplot(aes(x=`Donor Is Teacher`,y=`Donation Included Optional Donation`,fill=total))+geom_tile()+
  scale_fill_gradient2(low="blue",high = "red")+theme_bw()+labs(title="Relation between DIT and DIOD",fill="Counts")

prefix_amount<-inner_join(Donations,Projects,by="Project ID") %>% inner_join(Teacher,by="Teacher ID")
prefix_amount %<>% select(`Teacher Prefix`,`Donation Amount`,`Donation Included Optional Donation`) %>% filter(`Teacher Prefix` != "")
prefix_amount %>% group_by(`Teacher Prefix`,`Donation Included Optional Donation`) %>% summarise(total=mean(`Donation Amount`)) %>%
  ggplot(aes(x=`Teacher Prefix`,y=`Donation Included Optional Donation`,fill=total))+geom_tile()+
  scale_fill_continuous(low = "#84ffff",high = "#ff5722")+theme_bw()+labs(title="Relation between TP and DIOD",fill="Donation Amount(mean)")

#
