setwd("E:/kaggle/AustinWeather")
library(ggplot2)
library(dplyr)
library(data.table)
library(Hmisc)
library(lubridate)
weather<-fread("austin_weather.csv")

#查看平均气温，风速以及温度的变化
weather<-weather %>% mutate(Year=as.factor(year(weather$Date)))
ggplot(weather,aes(x=Year,y=TempAvgF,fill=Year))+geom_boxplot()+ggtitle("不同年份的平均温度分布")
weather %>% na.omit() %>% ggplot(aes(x=Year,y=as.numeric(HumidityAvgPercent),fill=Year))+geom_violin()+ggtitle("不同年份的平均湿度分布")+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 8)
weather %>% na.omit() %>% ggplot(aes(x=Year,y=as.numeric(WindAvgMPH),fill=Year))+ggtitle("不同年份的平均湿度分布")+geom_boxplot(outlier.size = 0.5,outlier.shape = 8,notch=T)

event_Precipitation<-weather %>% select(PrecipitationSumInches,Events) %>% filter(PrecipitationSumInches!='0')
weather$Events[weather$PrecipitationSumInches=="T" & weather$Events==""]="Rain"
weather %>% filter(PrecipitationSumInches!='0') %>% ggplot(aes(x=Events))+geom_histogram(stat = "count")
