setwd("E:/kaggle/house price")
library(data.table)
library(dplyr)
library(mice)
library(Hmisc)
library(Amelia)
library(caret)
library(MASS)
library(ggplot2)
library(corrplot)
library(e1071)
raw_train<-fread("train.csv");raw_test<-fread("test.csv")
max_price<-max(raw_train$SalePrice)
raw_train$SalePrice<-raw_train$SalePrice/max(raw_train$SalePrice)
full<-bind_rows(raw_train,raw_test)
describe(full)
missmap(full)
#查看缺失值情况
na_sum<-sapply(full, function(x){
  sum(is.na(x))
})
table(sapply(full, function(x){class(x)}))
full<-full[,-c("PoolQC","Fence","MiscFeature","FireplaceQu","Alley")]
ggplot(full[1:1460,],aes(x=LotFrontage))+geom_histogram()
full$LotFrontage[is.na(full$LotFrontage)]<-median(full$LotFrontage,na.rm = T)

#something about garage
garage<-c("GarageType","GarageFinish","GarageQual","GarageCond")
Basm<-c("BsmtExposure","BsmtCond","BsmtQual","BsmtFinType2","BsmtFinType1")
for (i in c(garage,Basm) ){
  full[[i]][is.na(full[[i]])]<-"None"
}
prop.table(table(full$MasVnrType,useNA = "ifany"))
full$MasVnrType[is.na(full$MasVnrType)]<-"None"
full$MasVnrArea[is.na(full$MasVnrArea)]<-0
full$BsmtFinSF1[is.na(full$BsmtFinSF1)]<-0
table(paste(full$BsmtHalfBath,full$BsmtFullBath,sep = "-"))
full$BsmtHalfBath[is.na(full$BsmtHalfBath)]<-0
full$BsmtFullBath[is.na(full$BsmtFullBath)]<-0
full$KitchenQual[is.na(full$KitchenQual)]<-"TA"
full$MSZoning[is.na(full$MSZoning)]<-"RL"
full$Exterior1st[is.na(full$Exterior1st)]<-"HdBoard"
full$TotalBsmtSF[is.na(full$TotalBsmtSF)]<-0
full$BsmtUnfSF[is.na(full$BsmtUnfSF)]<-0
full$Electrical[is.na(full$Electrical)]<-"SBrkr"
full$Exterior2nd[is.na(full$Exterior2nd)]<-"VinylSd"
full$Functional[is.na(full$Functional)]<-"Typ"
full$GarageCars[is.na(full$GarageCars)]<-0
full$GarageArea[is.na(full$GarageArea)]<-0
full$SaleType[is.na(full$SaleType)]<-"WD"
full$GarageYrBlt[is.na(full$GarageYrBlt)]<-full$YearBuilt[is.na(full$GarageYrBlt)]
full$Utilities<-NULL
full$BsmtFinSF2[is.na(full$BsmtFinSF2)]<-0
full$StFlrSF<-full$`1stFlrSF`
full$NdFlrSF<-full$`2ndFlrSF`
full$SsnPorch<-full$`3SsnPorch`
full$`1stFlrSF`<-NULL
full$`2ndFlrSF`<-NULL
full$`3SsnPorch`<-NULL
for (x in names(full)) {
  if(class(full[[x]])=="character"){
    full[[x]]<-as.factor(full[[x]])
  }
}
plot(full$StFlrSF,full$SalePrice,type = "p")
#将所有连续数值归一化
for (y in names(full)) {
  if(class(full[[y]])=="integer" & y!="Id" & y!="SalePrice"){
    full[[y]]<-(full[[y]]-min(full[[y]]))/(max(full[[y]])-min(full[[y]]))
  }
}
#构建线性回归模型
train1<-full[!is.na(full$SalePrice),]
train1<-train1[,-c("Id")]
test<-full[is.na(full$SalePrice),]
house.model<-randomForest(SalePrice~.,data = train1)
predict.house<-predict(house.model,test)
predict.house<-predict.house*max_price
result<-as.data.frame(cbind(test$Id,predict.house))
colnames(result)<-c("Id","SalePrice")
write.csv(result,"submission.csv",row.names = F)

house.svm<-train(SalePrice~.,data = train1,methods="rf",metric="RMSE")
predict.svm<-predict(house.svm,test)
predict.svm<-predict.svm*max_price
result1<-as.data.frame(cbind(test$Id,predict.svm))
colnames(result1)<-c("Id","SalePrice")
write.csv(result1,"submission.csv",row.names = F)
