#  Group           : We have two databases
#  HW              : Final Project
#  Algorithm       : ANN

#install.packages('NeuralNetTools')
#install.packages("stringr")
## remove all objects
rm(list=ls())
dev.off
library(neuralnet)
library(NeuralNetTools)
# read csv file
setwd("/Users/louyilin/RStudioProjects")
data<-read.csv("attrition_data.csv",header = TRUE,na.strings = "?")
data_keep=data
#  clean NA datas
k=na.omit(data)
# to much NA in TERMINATION_YEAR
data=data[,-14]
#data_keep$TERMINATION_YEAR[is.na(data_keep$TERMINATION_YEAR)]<-0

data=na.omit(data)

#print(typeof(data$ETHNICITY))
data$ETHNICITY <- as.numeric(data$ETHNICITY, levels = c("AMIND", "ASIAN", "BLACK", "HISPA", "PACIF", "TWO", "WHITE"), labels = c(1,2,3,4,5,6,7))
data$SEX=as.numeric(data$SEX, levels = c("F","M"),lable=c(1,2))
data$MARITAL_STATUS=as.numeric(data$MARITAL_STATUS, levels = c("Divorced","Single","Married"),lable=c(1,2,3))
data$NUMBER_OF_TEAM_CHANGED=as.numeric(data$NUMBER_OF_TEAM_CHANGED, levels = c("0","1","2","3","3+"),lable=c(0,1,2,3,4))
#gsub("e", "", group)
#view levels of EFERRAL_SOURCE
REFERRAL_SOURCE_Levels=levels(factor(data$REFERRAL_SOURCE))
REFERRAL_SOURCE_Levels=c(REFERRAL_SOURCE_Levels)
arr1 <- array(1:20)
data$REFERRAL_SOURCE=as.numeric(data$NUMBER_OF_TEAM_CHANGED, levels =REFERRAL_SOURCE_Levels,lable=arr1)
HIRE_MONTH_Levels=levels(factor(data$REFERRAL_SOURCE))
arr2<-array(1:12)
data$HIRE_MONTH=as.numeric(data$HIRE_MONTH, levels =HIRE_MONTH_Levels,lable=arr2)
data$REHIRE=as.numeric(data$REHIRE, levels = c(TRUE,FALSE),lable=c(1,2))
data$IS_FIRST_JOB=as.numeric(data$REHIRE, levels = c("N","Y"),lable=c(1,2))
data$TRAVELLED_REQUIRED=as.numeric(data$TRAVELLED_REQUIRED, levels = c("N","Y"),lable=c(1,2))
data$DISABLED_EMP=as.numeric(data$DISABLED_EMP, levels = c("N","Y"),lable=c(1,2))
data$DISABLED_VET=as.numeric(data$DISABLED_VET, levels = c("N","Y"),lable=c(1,2))
data$EDUCATION_LEVEL=as.numeric(data$EDUCATION_LEVEL, levels = c("LEVEL 1","LEVEL 2","LEVEL 3","LEVEL 4","LEVEL 5"),lable=c(1,2,3,4,5))
JOB_GROUP_Levels=levels(factor(data$JOB_GROUP))
JOB_GROUP_Levels=c(JOB_GROUP_Levels)
JOB_GROUP_Levels
arr3<-array(1:60)
data$JOB_GROUP=as.numeric(data$JOB_GROUP, levels =JOB_GROUP_Levels,lable=arr3)
Status_Levels=levels(factor(data$STATUS))
data$STATUS=as.numeric(data$STATUS, levels = c("T","A"),lable=c(1,2))
#drop some features that we dont need
newdata<-data[,-c(1,4)]

#############
##Use 30% test 70% training

idx <- sample(1:nrow(data),round(0.7*nrow(data)))
training<-data[idx,]
test<-data[-idx,]
max = apply(newdata, 2 , max)
min = apply(newdata, 2 , min)

# response var must be scaled to [0 < resp < 1]
annScaled<- as.data.frame(scale(newdata, center = min, scale = max- min ))
trainNN<-annScaled[idx,]
testNN<-annScaled[-idx,]

ANN<- neuralnet(STATUS~.,data=trainNN,hidden=c(6,3), threshold=0.2,act.fct = "logistic", linear.output = FALSE)
plot(ANN)
#A=as.numeric(ANN$net.result)
pred<-compute(ANN ,testNN[,-18])
pred$net.result
pred=(pred$net.result * (max(data[,18]) - min(data[,18]))) + min(data[,18])

for (i in 1: length(pred)) {
  pred[i]=round(pred[i])
}

accuracy= (test$STATUS==pred)


acc<-sum(accuracy)/length(accuracy)

cm=table(actual=test$STATUS,ANN=pred)
cm
prontential=cm[1]/(cm[1]+cm[3])
prontential
print(paste("the accuracy is",acc))
print(paste("the protential of employees leaving the company is: ",prontential))







