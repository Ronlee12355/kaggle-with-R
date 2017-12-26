setwd("E:/kaggle/mental-healthy")
lapply(c("tidyverse","data.table","stringr","randomForest","caret",'magrittr'),require,character.only = TRUE)
survey<-fread("survey.csv",stringsAsFactors = T)
sapply(survey,function(x){sum(is.na(x))})
glimpse(survey)
survey %<>% select(-c(comments,state,work_interfere))
survey %<>% select(-c(Timestamp))
survey %<>% select(-c(self_employed))
#数据中的年龄变化归一化
table(survey$Gender)
survey$Gender<-str_to_lower(survey$Gender)
male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man","cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", 
               "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")
survey$Gender <- if_else(survey$Gender %in% male_str,"male",survey$Gender)
survey$Gender <- if_else(survey$Gender %in% trans_str,"trans",survey$Gender)
survey$Gender <- if_else(survey$Gender %in% female_str,"female",survey$Gender)
survey %<>% filter(Gender != "p")
survey %<>% filter(Gender != "a little about you")
survey$Gender<-as.factor(survey$Gender)
table(survey$self_employed,useNA = "ifany")
survey$self_employed<-as.character(survey$self_employed)
survey %>% na.omit() %>% ggplot(aes(x=treatment,fill=self_employed))+geom_bar(position = "dodge")
survey$self_employed <- if_else(is.na(survey$self_employed),"Yes",survey$self_employed) %>% as.factor()
survey %<>% filter(Age>0 & Age<110)

train.data<-survey %>% sample_frac(0.7)
test.data<-survey %>% sample_frac(0.3)
final_result<-data.table("method"=c("randomForest","bagging","gbm","C50","rpart"),"precision"=rep(0,5),"recall"=rep(0,5))

#rf
rf.model<-randomForest(treatment~.,data = train.data,ntree=600,importance=T)
#varImpPlot(rf.model,main = "随机森林变量重要性排序")
p1=data.table("variable"=rownames(rf.model$importance),"overall"=rf.model$importance[,4]) %>%
  ggplot(aes(variable,overall,fill=factor(variable)))+geom_col()+coord_flip()+
  theme(legend.position = "none")+labs(x="变量",y="模型重要性",title="变量重要性排名(randomforest)")
rf.test<-predict(rf.model,test.data)
confusionMatrix(rf.test,test.data$treatment,mode="everything",positive = "Yes")
final_result$precision[1]<-0.90000;final_result$recall[1]<-0.05000
plot(precrec::evalmod(scores=predict(rf.model,test.data,type="prob")[,2],labels =test.data$treatment))

#bagging
bag.model<-adabag::bagging(treatment~.,data = train.data)
p2=data.table("variable"=names(bag.model$importance),"importance"=bag.model$importance) %>% 
  ggplot(aes(reorder(variable,importance),importance,fill=factor(variable)))+
  geom_col(width = 0.4)+coord_flip()+
  theme(legend.position = "none")+labs(x="变量",y="模型重要性",title="变量重要性排名(bagging)")
bag.test<-predict(bag.model,test.data)
confusionMatrix(bag.test$class,test.data$treatment,mode="everything",positive = "Yes")
final_result$precision[2]<-0.7071;final_result$recall[2]<-0.7778
plot(precrec::evalmod(scores=bag.test$prob[,2],labels =test.data$treatment))

#gbm
gbm.model<-train(treatment~.,data = train.data,method="gbm")
gbm.imp<-varImp(gbm.model)$importance
p3=data.table("variable"=rownames(gbm.imp),"overall"=gbm.imp$Overall) %>% filter(overall>0) %>%
  ggplot(aes(variable,overall,fill=factor(variable)))+geom_col()+coord_flip()+
  theme(legend.position = "none")+labs(x="变量",y="模型重要性",title="变量重要性排名(gbm)")
gbm.test<-predict(gbm.model,test.data)
confusionMatrix(gbm.test,test.data$treatment,mode="everything",positive = "Yes")
final_result$precision[3]<-0.7068;final_result$recall[3]<-0.75
plot(precrec::evalmod(scores=predict(gbm.model,test.data,type="prob")[,2],labels =test.data$treatment))

c50.model<-train(treatment~.,data = train.data,method="C5.0")
p4=data.table("variable"=rownames(varImp(c50.model)$importance),"overall"=varImp(c50.model)$importance$Overall) %>% filter(overall>0) %>%
  ggplot(aes(variable,overall,fill=factor(variable)))+geom_col()+coord_flip()+
  theme(legend.position = "none")+labs(x="变量",y="模型重要性",title="变量重要性排名(C5.0)")
c50.test<-predict(c50.model,test.data)
confusionMatrix(c50.test,test.data$treatment,mode="everything",positive = "Yes")
final_result$precision[4]<-0.8634;final_result$recall[4]<-0.8778
plot(precrec::evalmod(scores=predict(gbm.model,test.data,type="prob")[,2],labels =test.data$treatment))

rp.model<-train(treatment~.,data = train.data,method="rpart")
p5=data.table("variable"=rownames(varImp(rp.model)$importance),"overall"=varImp(rp.model)$importance$Overall) %>% filter(overall>0) %>%
  ggplot(aes(variable,overall,fill=factor(variable)))+geom_col()+coord_flip()+
  theme(legend.position = "none")+labs(x="变量",y="模型重要性",title="变量重要性排名(rpart)")
rp.test<-predict(rp.model,test.data)
confusionMatrix(rp.test,test.data$treatment,mode="everything",positive = "Yes")
final_result$precision[5]<-0.7034;final_result$recall[5]<-0.5667
plot(precrec::evalmod(scores=predict(rp.model,test.data,type="prob")[,2],labels =test.data$treatment))
rpart.plot::rpart.plot(rpart(treatment~.,data = train.data))

ggplot(final_result,aes(method,recall,fill=factor(method)))+geom_col(width = 0.5)+geom_text(aes(label=recall),vjust=-0.2)+
  labs(x="方法",y="recall数值",title="不同树算法的recall值高低")+
  theme(legend.position = "none")

ggplot(final_result,aes(method,precision,fill=factor(method)))+geom_col(width = 0.5)+geom_text(aes(label=precision),vjust=-0.2)+
  labs(x="方法",y="precision数值",title="不同树算法的precision值高低")+
  theme(legend.position = "none")
