#  Group           : We have two databases
#  HW              : Final Project
#  Algorithm       : Random

rm(list = ls())

library("randomForest")

# read csv file
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

set.seed(123)

# find the best mtry
# n <- length(names(training))
# rate = 1
# for (i in 1: (n - 1)) {
#   print(i)
#   set.seed(100)
#   rf = randomForest(as.factor(STATUS)~., data=training, ntree=1000, mtry=i)
#   rate[i] <- mean(as.numeric(rf$err.rate))
#   print(rf)
# }
# 
# rate
# plot(rate)

# 0.3832581 0.3314060 0.3091880 0.2943303 0.2839213 0.2763326 0.2706837 0.2703913 0.2650771 0.2645961 0.2652369 0.2616005
# 0.2621349 0.2632359 0.2611863 0.2611982 0.2621737 0.2618234 0.2624926 0.2627373 0.2641659 0.2599642 0.2641665 0.2637233
# 0.2632886

# which.min(rate)
# mtry = 22

# find the best ntree
# rf <- randomForest(as.factor(STATUS)~., data=training, mtry=22, importance=TRUE, ntree=1000)
# plot(rf)

# when trees equal 600, the model is becoming steady.

randomForest <- randomForest(as.factor(STATUS)~., data=training, mtry=22, importance=TRUE, ntree=600, proximity=TRUE)

# importance(randomForest)
# max ANNUAL_RATE
# min REHIRE

# varImpPlot(randomForest)

randomForestPrediction <- predict(randomForest, test)

table(actual=test$STATUS, Prediction=randomForestPrediction)

wrong <- (test$STATUS != randomForestPrediction)
randomForestRate = sum(wrong)/length(test$STATUS)

print(paste("Accuracy Rate is: ", (1-randomForestRate)*100))
# "Accuracy Rate is: 75"