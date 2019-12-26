hrdata=read.csv("C:\\Users\\Sudip\\Desktop\\machine learning\\HR_comma_sep.csv")
str(hrdata)
colSums(is.na(hrdata))
head(hrdata)
hrdata$salary=as.integer(as.factor(hrdata$salary))
hrdata$Department=as.integer(as.factor(hrdata$Department))
head(hrdata)
library(dplyr)
df=hrdata[,c(1,2,3,4,5,6,8,9,10,7)]
head(df)
d1=sample(2,nrow(df),replace = T,prob = c(.8,.2))
train=df[d1==1,]
test=df[d1==2,]
model=glm(left~.,data=train)
p=data.frame(predict(model,test[,-10]))
colnames(p)='new_left'
p$real_left=ifelse(p$new_left>.5,1,0)
c=cbind(test,p)
t=table(c$left,c$real_left)
f=sum(diag(t))/sum(t)
f
#accuracy is of 78% with Logistic Regression
df1=hrdata[,c(1,2,3,4,5,6,8,9,10,7)]
head(df1)
df1$left=as.factor(df1$left)
str(df1)
d2=sample(2,nrow(df1),replace = T,prob = c(.8,.2))
train1=df1[d2==1,]
test1=df1[d2==2,]
library(randomForest)
model1=randomForest(left~.,data=train1)
p1=data.frame(predict(model1,test1[,-10]))
colnames(p1)='new_left'
c1=cbind(test1,p1)
t1=table(c1$left,c1$new_left)
f1=sum(diag(t1))/sum(t1)
f1
#accuracy is of 99% with random forest classifier
