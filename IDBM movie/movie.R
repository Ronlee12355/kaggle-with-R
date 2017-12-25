setwd("E:/kaggle/IDBM movie")
library(data.table)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(corrplot)
library(plotly)
movie<-fread("movie_metadata.csv")
str(movie)
library(stringr)
movie$movie_title<-str_replace(movie$movie_title,"聽","")

#观察那几位导演的电影最多
director<-movie %>% na.omit() %>% select(director_name) %>% table() %>% as.data.frame()
colnames(director)<-c("name","times")
director_20<-director %>% arrange(desc(times)) %>% na.omit()
ggplot(director_20[1:20,],aes(x=reorder(name,times),y=times,alpha=times))+geom_bar(stat = "identity",fill="blue")+coord_flip()+xlab("电影数")+ylab("导演名字")+
  ggtitle("每个导演电影数")+scale_fill_brewer("RdBu")

#观察电影数量的变化
movie_sum<- movie %>% na.omit() %>% group_by(title_year) %>% summarise(total=n())
p<-movie_sum %>% ggplot(aes(x=title_year,y=total))+geom_line(color="red")+xlab("时间")+ylab("电影数目") +ggtitle("每一年的电影数")+
  geom_smooth(level=0,method = 'loess')
ggplotly(p)

#观察电影评分的变化
movie_score<- movie %>% na.omit() %>% group_by(title_year) %>% summarise(score=mean(imdb_score))
p1<-movie_score %>% ggplot(aes(x=title_year,y=score))+geom_line(color="red")+xlab("时间")+ylab("电影评分") +ggtitle("每一年的电影评分平均分")+
  geom_smooth(level=0)
ggplotly(p1)

#观察每个导演票房的变化
director_money<-movie %>% select(director_name,imdb_score)%>% group_by(director_name) %>% summarise(money=mean(imdb_score)) %>% arrange(desc(money))
director_money[1:10,] %>% ggplot(aes(x=director_name,y=money))+geom_bar(stat = "identity",fill="red")+xlab("时间")+ylab("电影评分") +ggtitle("每个导演的电影评分平均分")+
  geom_text(aes(label=money),vjust=0.1,color="darkblue")

rank_score<-movie %>% select(content_rating,imdb_score)
p2<-ggplot(rank_score,aes(x=content_rating,y=imdb_score,fill=factor(content_rating)))+geom_boxplot()+xlab("分数高低")+ylab("电影评分") +ggtitle("不同电影评级的电影评分平均分")
ggplotly(p2)

profit<-movie %>% na.omit() %>% select(title_year,budget,gross)
profit$diff<-profit$gross-profit$budget
a<-profit %>% group_by(title_year) %>% summarise(n=mean(diff))
p3<-ggplot(a,aes(x=title_year,y=n))+geom_line(color="blue")+geom_smooth(level=0,color="red")+xlab("时间")+ylab("年均票房收入平均数") +ggtitle("年均票房平均值")
ggplotly(p3)

movie %>% cor() %>% corrplot(type="lower",title="Corrgram of the movie data")
