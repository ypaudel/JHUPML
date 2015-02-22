setwd("C:/Users/Yudhisthir/Documents/MOOC_DataScienceModule/9_MachineLearning")

#Loading required packages
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)

##Getting Data
  train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  
  
  if(!file.exists("./ProjData/pml-training.csv")){
    download.file(url = train_url, destfile = "./ProjData/pml-training.csv")
  }
  if(!file.exists("./ProjData/pml-testing.csv")){
    download.file(url = test_url, destfile = "./ProjData/pml-testing.csv")
  }

##Cleaning Data
  training <- read.csv("./ProjData/pml-training.csv",na.strings=c("","NA","#DIV/0!"))
  testing <- read.csv("./ProjData/pml-testing.csv",na.strings=c("","NA","#DIV/0!"))
  
  names(training)
  dim(training)
  dim(testing)
  str(training)

  #Irrelevante variables (data descriptors)
  training  <-training[,-c(1:6)]
  testing <-testing[,-c(1:6)]

  # Delete columns with more than 25% of data missing/ "NAs"
  training <- training[,colSums(is.na(training)) < nrow(training)*0.75]
  testing <- testing[,colSums(is.na(testing)) < nrow(testing)*0.75]

  #Creating training and testing data from within given training data
    Training_cases <- createDataPartition(y=training$classe, p=0.60, list=FALSE)
    trainingSample <- training[Training_cases, ] 
    testingSample <- training[-Training_cases, ]

  #Plot of the Training data levels
  classe.plot <- ggplot(trainingSample, aes(classe))
  classe.plot + geom_bar() + xlab("Class levels") + ylab("Frequency") +
    ggtitle("Bar Plot of levels of the variable classe within the subTraining data set")

  ##Decision Tree Model
    dtModel <- rpart(classe ~ ., data=trainingSample, method="class")
    library(rattle)
  
    # Predicting
    dtPredict <- predict(dtModel, testingSample, type = "class")
    confusionMatrix(dtPredict, testingSample$classe)
  
  ##Random forest model
    rfModel <- randomForest(classe ~. , data=trainingSample, method="class")
  
    #Predicting
    rfPredict <- predict(rfModel, testingSample, type = "class")
    confusionMatrix(rfPredict, testingSample$classe)

##Final Testing (Submission) with 20 test cases
    finalPredict <- predict(rfModel, testing, type="class")
