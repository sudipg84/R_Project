library(readxl)
df=read_excel("C:\\Users\\Sudip\\Desktop\\machine learning\\Absenteeism_at_work.xls")
str(df)
colSums(is.na(df))
df$`Disciplinary_failure`= df$`Disciplinary failure`
df=df[,-12]
str(df)
df$Disciplinary_failure=as.factor(df$Disciplinary_failure)
d1=sample(2,nrow(df),replace = T,prob = c(.8,.2))
d1
train=df[d1==1,]
test=df[d1==2,]
library(e1071)
model=svm(Disciplinary_failure~.,data=train)
model
predicted=data.frame(predict(model,test[,-21]))
colnames(predicted)='new'
testdf=cbind(test,predicted)
tab=table(testdf$Disciplinary_failure,testdf$new)
final=sum(diag(tab))/sum(tab)
final
#accuracy is of 97%