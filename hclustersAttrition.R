#  Group           : We have two databases
#  HW              : Final Project
#  Algorithm       : Hcluster

## remove all objects
rm(list=ls())
dev.off
#read CSV
data<-read.csv("attrition_data.csv",header = TRUE,na.strings = "?")
#  clean NA datas
k=na.omit(data)
# to much NA in TERMINATION_YEAR
data=data[,-14]
#data_keep$TERMINATION_YEAR[is.na(data_keep$TERMINATION_YEAR)]<-0

data=na.omit(data)
l=levels(factor(data$REFERRAL_SOURCE))
l
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
standardize <- function(x){x/max(x)} 
hData=newdata
#  clean NA datas
standardize=na.omit(standardize)
#hData$type <- as.numeric(hData$type, levels = c("white", "red"), labels = c("1","2"))
hData[,-18] <- as.data.frame(lapply(hData[,-18], standardize))
hData[,18]=as.factor(hData[,18])
hCluster=hclust(dist(hData[,-c(18)]),method="average")
#divide into 2 clusters
plot(hCluster)
rect.hclust(hCluster, k = 6)
hClustCut<-cutree(hCluster,7)
hClusterTable=table(clusters=hClustCut,STATUS=hData[,'STATUS'])
print('hcluster Table and Plot')
print(hClusterTable)







