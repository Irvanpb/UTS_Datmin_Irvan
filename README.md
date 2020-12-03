# UTS_Datmin_Irvan
Ini UTS Datmin
data<-read.delim("http://archive.ics.uci.edu/ml/machine-learning-databases/undocumented/connectionist-bench/sonar/sonar.all-data",
                 header=FALSE,sep=",")


library(caret)
data$V61<-ifelse(data$V61=="R",0,1)
fungsi<-sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
train<-data[fungsi==1, ]
test<-data[fungsi==2, ]
crosval<-trainControl(method="repeatedcv",number=3,repeats=3)



library(arm)
fitmodel<-bayesglm(V61~.,data=train,family=binomial)
results<-predict(fitmodel,test,type="response")
V61Result<-ifelse(results>0.5,1,0)
table(V61Result,test$V61)
Confussion_Matrix<-table(V61Result,test$V61)
Confussion_Matrix
fitmodel
accuracy<-sum(diag(Confussion_Matrix))/sum(Confussion_Matrix)
accuracy
sensitivity<-Confussion_Matrix[1,1]/(Confussion_Matrix[1,1]+Confussion_Matrix[1,2])
sensitivity
specificity<-Confussion_Matrix[2,2]/(Confussion_Matrix[2,1]+Confussion_Matrix[2,2])
specificity
precision<-Confussion_Matrix[1,1]/(Confussion_Matrix[1,1]+Confussion_Matrix[2,1])
precision
recall<-sensitivity
Fscore<-2*precision*recall/(precision+recall)
Fscore
