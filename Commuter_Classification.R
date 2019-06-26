# Remove old variables and release memory
rm(list=ls())
gc()
memory.limit()

######### Function #########
# Get measures such as class accucary, lifts, Sensitivity, Specificity
get_measures <- function(label, prediction)
{
  
  result <- table(label, prediction) 
  # Get class accuracy
  AccuracySatisfied <- 100 * result[1] / (result[1]+result[3])
  AccuracyUnsatisfied <- 100 * result[4] / (result[2]+result[4])
  
  # Get lift
  tb1 <- table(label)
  LiftSatisfied <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
  LiftUnsatisfied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
  
  # Get Sensitivity and Specificity
  Sensitivity <- 100 * result[1] / (result[1]+result[2])
  Specificity <- 100 * result[4] / (result[3]+result[4]) 
  
  return(c("AccuracySatisfied" = AccuracySatisfied,"AccuracyUnsatisfied" = AccuracyUnsatisfied,
           "LiftSatisfied" = LiftSatisfied,"LiftUnsatisfied" = LiftUnsatisfied))
  
  
}
######### End Function #########


# Load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(e1071)
library(tree)

# Load data
mqdata <- read.csv("MqConnectData.csv")

dim(mqdata)
str(mqdata)
head(mqdata)

melt_data$TravelMethod[which((melt_data$TravelMethod %in% c("Drove car alone","Carpool","Taxi, Uber, etc.")))] <- "Car"


unique(mqdata$Satisfaction)
head(mqdata$Satisfaction[which(c("not applicable") %in% mqdata$Satisfaction)])
dim(mqdata)
head(test)

cleandata <- mqdata[which((mqdata$Satisfaction == "Satisfied") | (mqdata$Satisfaction == "Unsatisfied")  | (mqdata$Satisfaction == "Very satisfied")  | (mqdata$Satisfaction == "Very unsatisfied")),1:14]
dim(cleandata)

head(cleandata)

features <- c("AgeGroup","TravelMethod","Day","DepartureTime","ArrivalTime","Postcode","Long","Lat", "Satisfaction")
cleandata <- cleandata[,features]

head(cleandata)
unique(cleandata$Satisfaction)

## Add label column: Satisfied = 1, Unsatisfied = 0
cleandata$Label = "Unsatisfied"
cleandata$Label[which((cleandata$Satisfaction == "Satisfied") | (cleandata$Satisfaction == "Very satisfied"))] <- "Satisfied"

cleandata$Satisfied = 0
cleandata$Satisfied[which((cleandata$Satisfaction == "Satisfied") | (cleandata$Satisfaction == "Very satisfied"))] <- 1

# remove Satisfaction column
cleandata$Satisfaction <- NULL
dim(cleandata)
str(cleandata)
summary(cleandata)

write_csv(cleandata,path = "final_mq_connect.csv")

# Data input into Tableau for further processing

###############################################################################
# Load data from Tableau
cleandata_updated <- read.csv("final_mq_connect_tablueau.csv")

colnames <- c("Label","Satisfied","AgeGroup","MedianAge","TravelMethod","MethodNumber","Day","WeekDay","ArrivalHour","DepartureHour","Postcode","Lat","Long")
colnames(cleandata_updated) <- colnames

str(cleandata_updated)
selected_features <- c("Label","Satisfied","MedianAge","MethodNumber","WeekDay","ArrivalHour","DepartureHour","Lat","Long")
cleandata_updated <- cleandata_updated[, selected_features]
cleandata_updated$Lat <- as.integer(cleandata_updated$Lat)
cleandata_updated$Long <- as.integer(cleandata_updated$Long)

dim(cleandata_updated)
str(cleandata_updated)

# Remove NA values
cleandata_updated <- na.omit(cleandata_updated)

#########################################################
# DIVIDE into train, evaluation, and test data
cleandata <- cleandata_updated

nro <- nrow(cleandata)
set.seed(123456)
nid <- sample(1:nro,size=0.6*nro)
# Get train data
train <- cleandata[nid,] 
dim(train) # 11523     9
table(train$Label)

# Get evaluation data
test <- cleandata[-nid,]
tid <- sample(1:nrow(test),size=0.5*nrow(test))
# Get evaluation data
evaluation <- test[tid,]
dim(evaluation) # 3841    9
table(evaluation$Label)

# Get test data
test <- test[-tid,]
dim(test) # 3842    9
table(test$Label)

# Save data for model building and evaluation
write_csv(cleandata,path = "final_mq_connect.csv")
write_csv(train,path = "final_mq_train.csv")
write_csv(evaluation,path = "final_mq_evaluation.csv")
write_csv(test,path = "final_mq_test.csv")


##############################################################
# SVM TUNE MODEL => NOT BETTER THAN NOMAL SVM

# Get new train data
train.svm <- train
train.svm$Satisfied <- NULL

(table(train.svm$Label))
weight <- c("Satisfied"=1,"Unsatisfied"=1)
#weight <- c("Satisfied"=1,"Unsatisfied"=1.5)

# Fit svm model
(t1 <- format(Sys.time()))
model.svm <- svm(Label ~ ., data = train.svm, type="C-classification", 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial",class.weights = weight)
(t2 <- format(Sys.time()))
(difftime(t2,t1,units="mins")) # 0.7166667 mins

summary(model.svm) # Number of Support Vectors:  8623

pred.svm.train <- predict(model.svm, train.svm)
# Get class accuracy and lift
table(train.svm$Label, pred.svm.train) 
(get_measures(train.svm$Label, pred.svm.train))

## Check for evaluation data set
evaluation.svm <- evaluation
evaluation.svm$Satisfied <- NULL
pred.svm.evl <-(predict(model.svm, evaluation.svm))
# Get class accuracy and lift
(get_measures(evaluation.svm$Label, pred.svm.evl))

## Check for test data set
test.svm <- test
test.svm$Satisfied <- NULL
pred.svm.test <-(predict(model.svm, test.svm))
# Get class accuracy and lift
(get_measures(test.svm$Label, pred.svm.test))


# SVM TUNE MODEL

# Tune svm model
(t1 <- format(Sys.time()))
# Set a seed for replicable results
set.seed(123)
tune.obj <- tune.svm(Label ~ ., data = train.svm[1:1000,],
                     gamma = 10^(-2:-1), cost = 10^(1:3))
(t2 <- format(Sys.time()))
(difftime(t2,t1,units="mins"))

summary(tune.obj)
# - best parameters:
#   gamma cost
# 0.01  100

# Fit svm model with best parameters
(t1 <- format(Sys.time()))
model.svm <- svm(Label ~ ., data = train.svm, type="C-classification", 
                 cost=100, gamma=0.01, scale=TRUE, kernel="radial",class.weights = weight)
(t2 <- format(Sys.time()))
(difftime(t2,t1,units="mins")) # 0.7166667 mins

summary(model.svm) # Number of Support Vectors:  8623

pred.svm.train <- predict(model.svm, train.svm)
# Get class accuracy and lift
table(train.svm$Label, pred.svm.train) 
(get_measures(train.svm$Label, pred.svm.train))

## Check for evaluation data set
pred.svm.evl <-(predict(model.svm, evaluation))
# Get class accuracy and lift
(get_measures(evaluation$Label, pred.svm.evl))

## Check for test data set
pred.svm.test <-(predict(model.svm, test))
# Get class accuracy and lift
(get_measures(test$Label, pred.svm.test))




##################################################################
# PLOT ROC CURVES OF BEST SVM MODELS

#install.packages("ROCR")
library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

# Rerun the best SVM model with decision.values=TRUE
# (t1 <- format(Sys.time()))
# model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", decision.values=T,
#                  cost=1000, gamma=0.1, scale=TRUE, kernel="radial", class.weights = weight)
# (t2 <- format(Sys.time()))

(t1 <- format(Sys.time()))
model.svm <- svm(Label ~ ., data = train.svm, type="C-classification",  decision.values=T,
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial",class.weights = weight)
(t2 <- format(Sys.time()))


summary(model.svm)

# Prepare data
fitted.train <- attributes(predict(model.svm,train.svm,decision.values=TRUE))$decision.values
fitted.evalu <- attributes(predict(model.svm,evaluation.svm,decision.values=TRUE))$decision.values
fitted.test <- attributes(predict(model.svm,test.svm,decision.values=TRUE))$decision.values

# fitted.train <- 1 - fitted.train
# fitted.evalu <- 1 - fitted.evalu
# fitted.test <- 1 - fitted.test

# Plot ROC curves
rocplot(fitted.train,train.svm[,"Label"],main="ROC Curves of SVM model",col="black",cex =.2)
rocplot(fitted.evalu,evaluation.svm[,"Label"],add=T,col="red")
rocplot(fitted.test,test.svm[,"Label"],add=T,col="blue")
legend("bottomright",legend =c("Train" ,"Evaluation","Test"),
       col=c("black","red","blue"),lty =1, lwd =2, cex =.8)

####### END ####### 


















##### DECISION TREE

train.nn <- train
train.nn$Label <- NULL
# Build tree with 10 folds cross-validation
set.seed(123)
model.tree = tree(Satisfied~ ., train.nn)
(t1 <- format(Sys.time()))
cv = cv.tree(model.tree,K=10)
(t2 <- format(Sys.time()))
(difftime(t2,t1,units="mins")) # 0 mins

qplot(cv$size, cv$dev) + geom_line(colour="blue", size=1)
cv$size[which.min(cv$dev)]

prune.tree <- prune.tree(model.tree, best=cv$size[which.min(cv$dev)])

options(digits=7)
{plot(prune.tree)
  text(prune.tree, pretty=0)}
options(digits=7)
# Train MSE
predict.train = predict(prune.tree, newdata=train.nn)
predict.train <- ifelse(predict.train > 0.5, 1, 0)
(get_measures(train.nn$Satisfied, predict.train))

## Check for evaluation data set
eval.nn <- evaluation
eval.nn$Label <- NULL
predict.eval = predict(prune.tree, newdata=eval.nn)
predict.eval <- ifelse(predict.eval > 0.5, 1, 0)
(get_measures(eval.nn$Satisfied, predict.eval))

## Check for test data set
test.nn <- test
test.nn$Label <- NULL
predict.test = predict(prune.tree, newdata=test.nn)
predict.test <- ifelse(predict.test > 0.5, 1, 0)
(get_measures(test.nn$Satisfied, predict.test))



##### DECISION TREE 2: CATEGORICAL FEATURES


# Load data from Tableau
cleandata_updated <- read.csv("final_mq_connect_tablueau.csv")

colnames <- c("Label","Satisfied","AgeGroup","MedianAge","TravelMethod","MethodNumber","Day","WeekDay","ArrivalHour","DepartureHour","Postcode","Lat","Long")
colnames(cleandata_updated) <- colnames

str(cleandata_updated)
selected_features <- c("Label","AgeGroup","TravelMethod","Day","ArrivalHour","DepartureHour","Lat","Long")
cleandata_updated2 <- cleandata_updated[, selected_features]
cleandata_updated2$Lat <- as.integer(cleandata_updated$Lat)
cleandata_updated2$Long <- as.integer(cleandata_updated$Long)

dim(cleandata_updated2)
str(cleandata_updated2)

# Remove NA values
cleandata_updated2 <- na.omit(cleandata_updated2)

#########################################################
# DIVIDE into train, evaluation, and test data
cleandata2 <- cleandata_updated2

nro <- nrow(cleandata2)
set.seed(123456)
nid <- sample(1:nro,size=0.6*nro)
# Get train data
train2 <- cleandata2[nid,] 
# Get evaluation data
test2 <- cleandata2[-nid,]
tid <- sample(1:nrow(test2),size=0.5*nrow(test2))
# Get evaluation data
evaluation2 <- test2[tid,]
# Get test data
test2 <- test2[-tid,]

# set parameters for decision tree
# ?tree.control
ctrl <- tree.control(nobs = 12000, mincut = 500, minsize = 1000, mindev = 0)

# build decision tree
tr1 <- tree(Label ~.,
            control = ctrl,
            data = train2)

# cross-validation
# FUN = prune.misclass is another pruning option (uses classification error
# instead of deviance for pruning)
cv <- cv.tree(tr1, K=10)
cv.tr1 <- cv

plot(cv.tr1$size, cv.tr1$dev, type = "b")
plot(cv.tr1$k, cv.tr1$dev, type = "b")

qplot(cv$size, cv$dev) + geom_line(colour="blue", size=1)
cv$size[which.min(cv$dev)]

# prune tree based on results of cross-validation
tr3 <- prune.tree(tr1, best = 9)
plot(tr3,
     type = c("uniform"))
text(tr3,
     pretty = 0,
     cex = 0.6)


prune.tree <- tr3
# Train Accuracy
predict.train = predict(prune.tree, newdata=train2,  type = "class")
(get_measures(train2$Label, predict.train))

## Check for evaluation data set
predict.eval = predict(prune.tree, newdata=evaluation2,  type = "class")
(get_measures(evaluation2$Label, predict.eval))

## Check for test data set
predict.test = predict(prune.tree, newdata=test2,  type = "class")
(get_measures(test2$Label, predict.test))

















###############################################
# NEURAL NETWORK
library(neuralnet)

# Fit neural network
# note that full formula needs to be written for neuralnet
# and outcome has to be in numerical format
train.nn <- na.omit(train.nn)

# Get 1% of Train data to build neural network
set.seed(123)
train.rows = sample(1:nrow(train.nn), nrow(train.nn)*0.015)

(t1 <- format(Sys.time()))
set.seed(123)
mmodel.nn <- neuralnet(Satisfied ~ MedianAge + MethodNumber + WeekDay + ArrivalHour + DepartureHour + Lat + Long, 
                       data = train.nn[train.rows,], hidden =  c(2,3), err.fct = "sse")  
# try others e.g. hidden = c(2,1)
(t2 <- format(Sys.time()))
(difftime(t2,t1,units="mins")) # 0.03333333333 mins

# plot result
plot(mmodel.nn)

predict.train <- compute(mmodel.nn, train.nn[,2:8]) 
predict.train <- predict.train$net.result
predict.train <- ifelse(predict.train > 0.5, 1, 0)
(get_measures(train.nn$Satisfied, predict.train))

## Check for evaluation data set
eval.nn <- evaluation
eval.nn$Label <- NULL
predict.eval <- compute(mmodel.nn, eval.nn[,2:8]) 
predict.eval <- predict.eval$net.result
predict.eval <- ifelse(predict.eval > 0.5, 1, 0)
(get_measures(eval.nn$Satisfied, predict.eval))

## Check for test data set
test.nn <- test
test.nn$Label <- NULL
predict.test <- compute(mmodel.nn, test.nn[,2:8]) 
predict.test <- predict.test$net.result
predict.test <- ifelse(predict.test > 0.5, 1, 0)
(get_measures(test.nn$Satisfied, predict.test))




######################################
# Deep Learning with TensorFlow
devtools::install_github("rstudio/tfestimators")
library(tfestimators)
install_tensorflow()

# Load data from Tableau
cleandata_updated <- read.csv("final_mq_connect_tablueau.csv")
colnames <- c("Label","Satisfied","AgeGroup","MedianAge","TravelMethod","MethodNumber","Day","WeekDay","ArrivalHour","DepartureHour","Postcode","Lat","Long")
colnames(cleandata_updated) <- colnames
selected_features <- c("Label","Satisfied","MedianAge","MethodNumber","WeekDay","ArrivalHour","DepartureHour","Lat","Long")
cleandata_updated <- cleandata_updated[, selected_features]
cleandata_updated$Lat <- as.integer(cleandata_updated$Lat)
cleandata_updated$Long <- as.integer(cleandata_updated$Long)
# Remove NA values
cleandata_updated <- na.omit(cleandata_updated)

#########################################################
# DIVIDE into train, evaluation, and test data
cleandata <- cleandata_updated

nro <- nrow(cleandata)
set.seed(123456)
nid <- sample(1:nro,size=0.6*nro)
# Get train data
train.dl <- cleandata[nid,] 
# Get evaluation data
test <- cleandata[-nid,]
tid <- sample(1:nrow(test),size=0.5*nrow(test))
# Get evaluation data
evaluation.dl <- test[tid,]
# Get test data
test.dl <- test[-tid,]







