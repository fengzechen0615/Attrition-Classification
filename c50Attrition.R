#  Group           : We have two datasets
#  HW              : Final Project
#  Algorithm       : C50

rm(list = ls())

library('C50')

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
newdata <- data[, -c(1, 4)]

##Use 30% test 70% training

idx <- sample(1:nrow(data),round(0.7*nrow(data)))
training <- data[idx,]
test <- data[-idx,]

set.seed(123)

# fit <- C5.0(as.factor(STATUS)~., data=training, 
#              control = C5.0Control(winnow = TRUE))
# summary(fit)

# find the best cf value

# err.rate <- function(training, test){
#   alpha <- NULL
#   res <- NULL
#   for (i in seq(0.25, 0.1, -0.01)){
#     fit <- C5.0(as.factor(STATUS)~., data=training, control = C5.0Control(CF = i))
#     pred <- predict(fit, test)
#     freq <- table(pred, test$STATUS)
#     accuracy <- sum(diag(freq))/sum(freq)
#     alpha <- c(alpha,i)
#     res <- c(res,accuracy)
#   }
#   return(data.frame(alpha, res))
# }
# err <- err.rate(training, test)

# alpha       res
# 1   0.25 0.7229542
# 2   0.24 0.7229542
# 3   0.23 0.7229542
# 4   0.22 0.7229542
# 5   0.21 0.7229542
# 6   0.20 0.7229542
# 7   0.19 0.7229542
# 8   0.18 0.7229542
# 9   0.17 0.7229542
# 10  0.16 0.7229542
# 11  0.15 0.7229542
# 12  0.14 0.7229542
# 13  0.13 0.7229542
# 14  0.12 0.7229542
# 15  0.11 0.7243412
# 16  0.10 0.7243412

# CF = 0.11

cost <- matrix(c(0, 2, 1, 0), nrow = 2, byrow = TRUE)

C50Class <- C5.0(as.factor(STATUS)~., data=training, control=C5.0Control(CF = 0.11), cost=cost, trials=30)
# C50Class <- C5.0(as.factor(STATUS)~., data=training, control=C5.0Control(CF = 0.11), trials=30)

C50Predict <- predict(C50Class, test, type="class")
cm <- table(actual=test$STATUS, C50=C50Predict)

protential = cm[1]/(cm[1] + cm[3])

wrong <- (test$STATUS != C50Predict)
C50Rate <- sum(wrong)/length(test$STATUS)

print(paste("Accuracy Rate is: ", (1-C50Rate)*100))
print(paste("The protential of employees leaving the company is: ", protential*100 ))