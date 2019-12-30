data=read.csv("C:\\Users\\Sudip\\Desktop\\machine learning\\AirQualityUCI.csv",sep = ';')
head(data)
data=data[,-c(16,17)]
head(data)
library(stringr)
data$AH=str_replace(data$AH,pattern = ',',replacement = ".")
data$RH=str_replace(data$RH,pattern = ',',replacement = '.')
data$T=str_replace(data$T,pattern = ',',replacement = '.')
data$CO.GT.=str_replace(data$CO.GT.,pattern = ',',replacement = '.')
data$C6H6.GT.=str_replace(data$C6H6.GT.,pattern = ',',replacement = '.')
head(data)
str(data)
data$CO.GT.=as.double(data$CO.GT.)
data$C6H6.GT.=as.double(data$C6H6.GT.)
data$T=as.double(data$T)
data$RH=as.double(data$RH)
data$AH=as.double(data$AH)
str(data)
colSums(is.na(data))
data=na.omit(data)
colSums(is.na(data))
dim(data)
data1=data[,-c(1,2)]
head(data1)
d1=sample(2,nrow(data1),replace = T,prob = c(.8,.2))
train=data1[d1==1,]
test=data1[d1==2,]
model=lm(AH~.,data=train)
summary(model)
p=data.frame(predict(model,test))
s=test[,13]
a=cbind(s,p)
cor(a$s,a$predict.model..test.)
#accuracy is of 99%