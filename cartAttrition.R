#  Group           : We have two datasets
#  HW              : Final Project
#  Algorithm       : CART

rm(list = ls())

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

# read csv file
data<-read.csv("attrition_data.csv", header = TRUE, na.strings = "?")
data_keep <- data
#  clean NA datas
k <- na.omit(data)
# to much NA in TERMINATION_YEAR
data <- data[, -14]
#data_keep$TERMINATION_YEAR[is.na(data_keep$TERMINATION_YEAR)]<-0

data <- na.omit(data)

#print(typeof(data$ETHNICITY))
data$ETHNICITY <- as.numeric(data$ETHNICITY, 
                             levels=c("AMIND", "ASIAN", "BLACK", "HISPA", "PACIF", "TWO", "WHITE"), 
                             labels=c(1, 2, 3, 4, 5, 6, 7))
data$SEX <- as.numeric(data$SEX, levels=c("F", "M"), labels=c(1, 2))
data$MARITAL_STATUS <- as.numeric(data$MARITAL_STATUS, 
                                  levels=c("Divorced", "Single", "Married"), 
                                  labels=c(1, 2, 3))
data$NUMBER_OF_TEAM_CHANGED <- as.numeric(data$NUMBER_OF_TEAM_CHANGED, 
                                          levels=c("0", "1", "2", "3", "3+"),
                                          labels=c(0, 1, 2, 3, 4))
#view levels of EFERRAL_SOURCE
REFERRAL_SOURCE_Levels <- levels(factor(data$REFERRAL_SOURCE))
arr1 <- array(1:20)
data$REFERRAL_SOURCE <- as.numeric(data$NUMBER_OF_TEAM_CHANGED, levels=REFERRAL_SOURCE_Levels, labels=arr1)
HIRE_MONTH_Levels <- levels(factor(data$HIRE_MONTH))
arr2 <- array(1:12)
data$HIRE_MONTH <- as.numeric(data$HIRE_MONTH, levels=HIRE_MONTH_Levels, labels=arr2)
data$REHIRE <- as.numeric(data$REHIRE, levels=c(TRUE, FALSE), labels=c(1, 2))
data$IS_FIRST_JOB <- as.numeric(data$REHIRE, levels=c("N", "Y"), labels=c(1, 2))
data$TRAVELLED_REQUIRED <- as.numeric(data$TRAVELLED_REQUIRED, levels=c("N", "Y"), labels=c(1, 2))
data$DISABLED_EMP <- as.numeric(data$DISABLED_EMP, levels=c("N", "Y"), labels=c(1, 2))
data$DISABLED_VET <- as.numeric(data$DISABLED_VET, levels=c("N", "Y"), labels=c(1, 2))
data$EDUCATION_LEVEL <- as.numeric(data$EDUCATION_LEVEL, 
                                   levels=c("LEVEL 1", "LEVEL 2", "LEVEL 3", "LEVEL 4", "LEVEL 5"),
                                   labels=c(1, 2, 3, 4, 5))
JOB_GROUP_Levels <- levels(factor(data$JOB_GROUP))
JOB_GROUP_Levels <- c(JOB_GROUP_Levels)
arr3 <- array(1: 60)
data$JOB_GROUP <- as.numeric(data$JOB_GROUP, levels=JOB_GROUP_Levels, labels=arr3)
data$STATUS <- factor(data$STATUS, levels=c("T", "A"), labels=c(1, 2))

#drop some features that we dont need
newdata<-data[,-c(1,4)]

#############
##Use 30% test 70% training

idx <- sample(1:nrow(data),round(0.7*nrow(data)))
training<-data[idx,]
test<-data[-idx,]

set.seed(123)

vec <- c(0, 2,
         1.2, 0)

cost <- matrix(vec, nrow = 2, byrow = TRUE)

cartClass <- rpart(as.factor(STATUS)~., data=training, parms=list(loss = cost))
# cartClass <- rpart(as.factor(STATUS)~., data=training)

# cartClass$cptable
cartPredict <- predict(cartClass, test, type='class')
cm <- table(Actual=test$STATUS, CART=cartPredict)

protential = cm[1]/(cm[1] + cm[3])

cartWrong <- sum(test$STATUS != cartPredict)
cartWrongRate <- cartWrong / length(test$STATUS)

print(paste("Accuracy Rate is: ", (1 - cartWrongRate) * 100))
print(paste("The potential of employees leaving the company is: ", protential*100 ))