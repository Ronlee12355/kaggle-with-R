#釜山行任务
library(rvest)
library(jiebaR)
library(stringr)
library(tidyverse)
library(network)
library(sna)
library(ggnetwork)
library(data.table)
library(igraph)
library(wordcloud2)
names_txt<-c("石宇","盛京","珍熙","露宿者","尚华","秀安","金常务","列车长","金代理","石宇的妈妈","乘务长","海英","少女","队员","英国","士兵","组长","前妻")
url<-"http://labfile.oss.aliyuncs.com/courses/677/busan.txt"
text<-read_html(url) %>% html_text() %>% str_split("\r\n\r\n")
text<-sapply(text,function(i){
  return(if_else(str_length(i)<=1,"NA",i))
})
for(j in 1:length(text)){
  if(text[j] == "NA"){
    text=text[-j]
  }
}
text<-as.list(text)
tmp_data<-list()
cutter<-worker(type = "mix")
for(i in 1:length(text)){
  tmp_data[[i]]<-cutter[text[[i]]][which(cutter[text[[i]]] %in% names_txt)]
}
final_data<- names_txt %>% combn(2) %>% t() %>% data.table()
colnames(final_data)<-c("name1","name2")
final_data<-final_data %>% mutate(bind_name=str_c(name1,name2,sep="--"),weights=rep(0,nrow(final_data)))
for(i in 1:624){
  if(length(tmp_data[[i]]) != 0){
    test=as.data.frame(table(paste(expand.grid(tmp_data[[i]],tmp_data[[i]])$Var1,
                                   expand.grid(tmp_data[[i]],tmp_data[[i]])$Var2,sep = '--')))
    final_data$weights[which(final_data$bind_name %in% test$Var1)]=final_data$weights[which(final_data$bind_name %in% test$Var1)]+test$Freq[which(test$Var1 %in% final_data$bind_name)]
  }
}
final_data<-final_data %>% filter(weights>0) %>% arrange(desc(weights))
text_c<-as.character(text)
index<-as.data.frame(table(cutter[text_c][which(cutter[text_c] %in% names_txt)])) %>% arrange(desc(Freq))
ind<-c()
p<-graph.data.frame(final_data[,-3],directed = F)
plot(p,edge.width = E(p)$weights,vertex.label.cex=0.9,vertex.label.color='black',layout=layout.fruchterman.reingold,vertex.color=index$Freq,edge.color = E(p)$weights)

#词云图
cutter[text_c] %>% table() %>% as.data.frame() %>% wordcloud2()
