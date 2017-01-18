library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

options(stringsAsFactors = FALSE)
## set wording directory
setwd("~/Workspace/shelterAnimals/")

source("./lib.R")

## load data
animal_train <- fread("./data/train.csv", na.strings = "")
animal_test <- fread("./data/test.csv", na.strings = "")
train_feature <- animal_train %>% featEng()
train_target <- animal_train$OutcomeType
test_feature <- animal_test %>% featEng()


library(doMC)
library(parallel)
registerDoMC(cores = detectCores())

library(caret)
library(pROC)
set.seed(123)
inTraining <- createDataPartition(train_muted$OutcomeType, p = .8, list = FALSE)
subTrain <- train_muted[inTraining,]
subTest <- train_muted[-inTraining,]

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = multiClassSummary,
                           search = "random")

set.seed(825)
gbmFit <- train(OutcomeType ~ ., data = subTrain, 
               method = "gbm",
               metric = "ROC",
               tuneLength = 50,
               trControl = fitControl)

