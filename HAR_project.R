#librery
library(caret)

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
inTrain<-createDataPartition(y = trainY,p = 0.6,list=F)
subTrain<-trainNumProc[inTrain,]
subTrainY<-trainY[inTrain]
subTest<-trainNumProc[-inTrain,]
subTestY<-trainY[-inTrain]

#train model with trees
modFit<-train(subTrainY~., method="rpart", data=subTrain)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=T, main="Classification Tree")
text(modFit$finalModel, use.n=T, all=T, cex=.8)

confusionMatrix(predict(modFit, newdata = subTest),subTestY) #poor classification

#train model random forest
modFit2<-randomForest(subTrainY~., data=subTrain,mtry=dim(subTrain)[2],importance=TRUE)
confusionMatrix(predict(modFit2, newdata = subTest),subTestY) #99% accuracy

#function to write files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

#make prediction with model
answers=as.character(predict(modFit2,newdata=testing))
setwd(dir = "./projectAnswers/")
pml_write_files(answers)


