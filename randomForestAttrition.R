#  Group           : We have two datasets
#  HW              : Final Project
#  Algorithm       : Random

rm(list = ls())

library("randomForest")

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
newdata<-data[, -c(1,4)]

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
#   rf = randomForest(as.factor(STATUS)~., data=training, mtry=i)
#   rate[i] <- mean(as.numeric(rf$err.rate))
#   print(rf)
# }
# 
# rate
# plot(rate)

# 0.3879041 0.3309677 0.3056223 0.2916020 0.2840462 0.2757956 0.2734508 0.2706642 0.2626486 0.2626204 0.2632365
# 0.2592803 0.2586202 0.2624443 0.2563500 0.2603950 0.2575627 0.2601820 0.2583465 0.2608151 0.2615266 0.2635326
# 0.2615647 0.2639324 0.2630074

# which.min(rate)
# mtry = 15

# find the best ntree
# rf <- randomForest(as.factor(STATUS)~., data=training, mtry=15, importance=TRUE, ntree=1000)
# plot(rf)

# when trees equal 600, the model is becoming steady.

randomForest <- randomForest(as.factor(STATUS)~., data=training, mtry=15, importance=TRUE, ntree=600, proximity=TRUE)

# importance(randomForest)
# max ANNUAL_RATE
# min REHIRE

# varImpPlot(randomForest)

randomForestPrediction <- predict(randomForest, test)

cm <- table(actual=test$STATUS, Prediction=randomForestPrediction)
protential = cm[1]/(cm[1] + cm[3])

wrong <- (test$STATUS != randomForestPrediction)
randomForestRate = sum(wrong)/length(test$STATUS)

print(paste("Accuracy Rate is: ", (1-randomForestRate)*100))
print(paste("The potential of employees leaving the company is: ", protential*100 ))