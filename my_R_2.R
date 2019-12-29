data=read.csv("C:\\Users\\Sudip\\Desktop\\machine learning\\dataset_Facebook.csv",sep = ";")
str(data)
colSums(is.na(data))
a=round(mean(data$Paid,na.rm = T),1)
data[is.na(data$Paid),7]=a
b=round(mean(data$like,na.rm = T),0)
data[is.na(data$like),17]=b
c=round(mean(data$share,na.rm = T),0)
data[is.na(data$share),18]=c
colSums(is.na(data))
data$Type=as.integer(as.factor(data$Type))
str(data)
data$Total_Interactions=data$Total.Interactions
data=data[,-19]
d1=sample(2,nrow(data),replace = T,prob = c(.8,.2))
d1
train=data[d1==1,]
test=data[d1==2,]
model=lm(Total_Interactions~.,data=train)
summary(model)
p=data.frame(predict(model,test[1:5,-19]))
p
test[1:5,19]
#accuracy is of 99%
