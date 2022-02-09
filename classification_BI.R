library(magrittr)
library(dplyr)
library(e1071)
library(caret)

drugs=read.csv("drug200.csv",header = T)

#Target variable is the drug type A,B,C,X,Y
table(drugs$Drug)


# Function to check NA
missingValues=function(data){
  count=0
  a=cbind(lapply(lapply(data, is.na), sum))
  for(i in 1:ncol(data)){
    if(a[i]!=0){
      cat("There are", a[i], "missing values in column ", i,"\n" )
      count=count+1
    }
  }  
  if(count==0){
    cat("There are No missing values in this dataset")
  }
}

missingValues(drugs)


# Split into train and test
set.seed(17)
index=nrow(drugs)
trainSet=(1:index)%in%sample(index,floor(index*0.7))
testSet=!trainSet


#build the model
model=naiveBayes(Drug~.,data=drugs,subset=trainSet)
model

#posterior distribution
predict(model,drugs[trainSet,],type="raw")[1,]*100
#We can see that the prediction for the first observation on the test set is 100% for de DRUG Y
predict(model,drugs[trainSet,],type="raw")[7,]*100
#For the seventh obsevation, we can see that the prediction is 77% for drug C, so it is the most likelely drug for that patient 


#Maximum a posteriori
drug_prediction=predict(model,drugs[testSet,])
#Now we see the drug prediction for each variable

#Validate the model
table(drug_prediction,drugs$Drug[testSet])
sum(testset)
#Now we can see how well our model classifies for our test set, there are only 3 missclasification, 2 for saying drugC when it is X and another one when it is Y


#PLot continuous variables
mms=apply(drugs[,c(-2,-3,-4,-6)],2,function(x) unlist(by(x,drugs$Drug,mean)))
sds=apply(drugs[,c(-2,-3,-4,-6)],2,function(x) unlist(by(x,drugs$Drug,sd)))

par(mfrow=c(1,2))
for(i in 1:2){
  rrx=range(c(mms[,i]+3*sds[,i],mms[,i]-3*sds[,i]))
  rry=c(0,max(dnorm(mms[,i],mms[,i],sds[,i]+0.0001)))
  plot(0,1,type="n",xlab=colnames(mms)[i],ylab="Normal Density",xlim=rrx,ylim=rry)
  ss=seq(rrx[1],rrx[2],length.out = 100)
  for(j in 1:5) points(ss,dnorm(ss,mms[j,i],sds[j,i]),type="l",col=j)
}

datos=data(Glass)





