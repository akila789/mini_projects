
#Load required libraries
library(readxl)
library(anonymizer)
library(dplyr)
library(ROCR)
library(Amelia)
library(ggplot2)
library(caTools)
library(lubridate)


setwd("D:\\Learnings\\CDS")
  
#Read data into R workplace
df <- read_excel("CallCenterData.xlsx", sheet = "Sheet1")

#Checking for incomplete data in the dataset Amelia package
#missmap(df,'Checking missing data', col = c('yellow','black'))


#Selecting only records with 'DATE_COMMITED' and 'CLOSED_DATE' available.
df.complete <- subset(df , !is.na(DATE_COMMITTED) & !is.na(CLOSED_DATE))


#Get unique 'areas' into vector, convert it into data frame with key to identify each 'area'
uniqueAreasVector <- as.vector(unique(df.complete$AREA))
uniqueAreasDf <- data.frame(uniqueAreasVector)
uniqueAreasDf$AreaID <-seq.int(nrow(uniqueAreasDf))

#Get unique 'Sub areas' into vector, convert it into data frame with key to identify each 'sub area'
uniqueSubAreasVector <- as.vector(unique(df.complete$SUB_AREA))
uniqueSubAreaDf <- data.frame(uniqueSubAreasVector)
uniqueSubAreaDf$SubAreaID <- seq.int(nrow(uniqueSubAreaDf))

#Get unique 'Owner group' into vector, convert it into data frame with key to identify each 'Owner group'
uniqueOwnerGroupVector <- as.vector(unique(df.complete$OWNER_GROUP))
uniqueOwnerGroupDf <- data.frame(uniqueOwnerGroupVector)
uniqueOwnerGroupDf$OwnerGroupID <- seq.int(nrow(uniqueOwnerGroupDf))



#Merge those data frames into main data frame
df2 <- merge(df.complete,uniqueAreasDf, by.x = "AREA", by.y = "uniqueAreasVector")
df2 <- merge(df2, uniqueSubAreaDf, by.x ="SUB_AREA", by.y ="uniqueSubAreasVector")
df2 <- merge(df2, uniqueOwnerGroupDf, by.x ="OWNER_GROUP", by.y ="uniqueOwnerGroupVector")

#Setting a seed value for reproducability
set.seed(1)

#Anonymize the sensitive data
df2$SRN <- anonymize(df2$`SR#`, .n_chars = 2L)
df2$MobileNo <- anonymize(df2$`MOBILE#`, .n_chars = 2L)

#Creating new data frame with selected columns
df3 <- select(df2, c('SRN','MobileNo','OwnerGroupID','AreaID','SubAreaID','DATE_COMMITTED','CLOSED_DATE',
                     'SR_TYPE','CONNECTION_TYPE','STATUS','SUB_STATUS'))


#Data type conversions
df3$SUB_STATUS <- as.character(df3$SUB_STATUS)
df3$AreaID <- as.character(df3$AreaID)
df3$SubAreaID <- as.character(df3$SubAreaID)
df3$OwnerGroupID <- as.character(df3$SubAreaID)
df3$DATE_COMMITTED <- dmy(df3$DATE_COMMITTED)
df3$CLOSED_DATE <- dmy(df3$CLOSED_DATE)



#Function for identify the case closed before or not
NotResolvedOnTime <- function(commited, closed ){
  numOfDays = (commited - closed)
  return(if_else(numOfDays >= 0, 0, 1))
 
}


#Adding new categorical variable. We use this variable as dependant variable
df3$NotResolvedOnTime = NotResolvedOnTime(commited = df3$DATE_COMMITTED, closed = df3$CLOSED_DATE)
df3$NotResolvedOnTime = as.factor(df3$NotResolvedOnTime)


ggplot(df3, aes(NotResolvedOnTime)) + 
  geom_bar(aes(fill=factor(NotResolvedOnTime)), alpha = 0.5) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  scale_fill_discrete(name = 'Not resolved on time') +
  ggtitle('Inquiries not resolved on time')


#Putting owner groups into bins. This used to avoid overfitting. 
df3$OwnerGroupRanges<- cut(as.numeric(df3$OwnerGroupID), breaks = c(0,10,20,30,40,50,70,80), labels = c(1,2,3,4,5,6,7))

#Get the weekday from date
df3$closed_day <-weekdays(as.Date(df3$CLOSED_DATE))
df3$commited_day <- weekdays(as.Date(df3$DATE_COMMITTED))


#write.csv(df3,file = 'df3V3.csv')

#Creating logistic regression model
set.seed(1)
split = sample.split(df3$NotResolvedOnTime, SplitRatio = 0.7)

df.train = subset(df3, split==TRUE)
df.test = subset(df3, split == FALSE)

RegressionModel = glm(NotResolvedOnTime ~ OwnerGroupRanges + AreaID + closed_day , data = df.train, family = binomial)

summary(RegressionModel)


#How to find good thresholding value
fitted.probabilities = predict(RegressionModel,newdata = df.test,type ='response')


#Finding a threshold value
ROCRPred = prediction(fitted.probabilities, df.test$NotResolvedOnTime)
ROCRPerf = performance(ROCRPred,'tpr','fpr')
plot(ROCRPerf,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

table(df.test$NotResolvedOnTime, fitted.probabilities > 0.2)

#Accuracy
(405+56) / (405+56+56+33) ##0.83

#Sensitivity TP/(TP + FN)
56 / (56+33) # 0.62

#Specifity TN/(TN + FP)
405/(405+56) # 0.87 


