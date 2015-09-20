#Dowload data
setwd("/home/gab/Documents/DataAnalysis/DataScience/machineLearning")
urlTr<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTs<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url = urlTr, destfile = "./training.csv", method = "curl")
download.file(urlTs,destfile = "./test.csv", method="curl")
#read files into R, using na.string after a previous overview of the dataset
training<-read.csv(file = "./training.csv",na.strings=c("NA","#DIV/0!",""))
testing<-read.csv("./test.csv",na.strings=c("NA","#DIV/0!",""))
#expolre database avg_pitch_bel
str(training)
str(testing)

#eliminate variables with many NA's >60
RatioNA=function(x) sum(is.na(x))/length(x)
PercNA<-apply(X = training, 2, FUN = RatioNA)
train<-training[,-which(PercNA>0.6)]
test<-testing[,-which(PercNA>0.6)]

#eliminate first 6 covariate not numeric and the last outcome variable
numData=function(x) {
    l<-dim(x)[2]
    x[,-c(1:6,l)] 
}
trainNum<-numData(train)
trainY<-train$classe
testNum<-numData(test)

#pre process variables
preObj<-preProcess(trainNum, method=c("knnImpute","center","scale"))
trainNumProc<-predict(preObj,trainNum)

#check for near zero variance
nzv<-nearZeroVar(trainNumProc,saveMetrics = T)
sum(nzv$nzv) #there is no variable with near zero variance

testLast<-predict(preObj, testNum)

#cross validation - slice the dataset in sub train and sub test
library(caret)
inTrain<-createDataPartition(y = trainY,p = 0.6,list=F)
subTrain<-trainNumProc[inTrain,]
subTest<-trainNumProc[-inTrain,]



