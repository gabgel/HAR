#librery
library(caret)
set.seed(1234)
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

#eliminate variables with many NA's >30
RatioNA=function(x) sum(is.na(x))/length(x)
PercNA<-apply(X = training, 2, FUN = RatioNA)
train<-training[,-which(PercNA>0.3)]


#eliminate first 7 covariate not numeric and the last outcome variable
numData=function(x) x[,-c(1:7)] 
trainNum<-numData(train)


#check for near zero variance
l<-dim(trainNum)[2]
nzv<-nearZeroVar(trainNum[,-l],saveMetrics = T)
sum(nzv$nzv) #there is no variable with near zero variance


#cross validation - slice the dataset in sub train and sub test
inTrain<-createDataPartition(y = trainNum$classe,p = 0.6,list=F)
subTrain<-trainNum[inTrain,]
subTest<-trainNum[-inTrain,]

#train classification trees
modFitTree<-train(classe~. , data=subTrain, method="rpart",
                  preProcess=c("knnImpute","center","scale"))

confusionMatrix(predict(modFitTree, newdata = subTest),subTest$classe) #poor one


#train model random forest
library(randomForest); 
modFit<-randomForest(classe~. , preProcess=c("knnImpute","center","scale"),
                     data=subTrain, importance=TRUE)

confusionMatrix(predict(modFit, newdata = subTest),subTest$classe) #99% accuracy
print(modFit)

#function to write files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

#make prediction with model
answers=predict(modFit,newdata=testing)
setwd(dir = "./projectAnswers/")
pml_write_files(answers)
