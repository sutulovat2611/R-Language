# Author: Sutulova Tatiana, 30806151
# Assignment 2
# The objective of this assignment is to gain familiarity with classification models using R.
# We want to obtain a model that may be used to predict whether or not it will be more humid
# tomorrow than it is today for 10 locations in Australia.

# Libraries and packages to be used
library(dplyr)
library(tree) # For decision tree 
library(e1071) # For Naive Bayes 1
library(adabag) # For bagging
library(rpart) # For bagging and boosting
library(randomForest) # For random forest
library(ROCR) # For ROCR and AUC

detach("package:neuralnet", unload = TRUE) 
# Read the data from the file into a data frame
rm (list = ls())
WAUS <- read.csv("HumidPredict2023D.csv", header = TRUE)
L <- as.data.frame(c(1:49))
set.seed(30806151) 
L <- L[sample(nrow(L), 15, replace = FALSE),] # sample 15 locations (reduced when processing)
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

# Q2. Pre-processing: 
WAUS <- unique(WAUS) # Clear out duplicates
WAUS <- na.omit(WAUS) # Removing rows with NA values

unique_val <- unique(WAUS$Location) # checking whether there are 10 distinct locations

################################################################################
# Q1: Proportion of proportion of days when it is more humid than the previous day compared to those where it is less humid?
length(WAUS$MHT[WAUS$MHT == 1]) # More humid tomorrow
length(WAUS$MHT[WAUS$MHT == 0]) # Less humid tomorrow

# Viaualising with pie chart
pct <- c(length(WAUS$MHT[WAUS$MHT == 1]), length(WAUS$MHT[WAUS$MHT == 0]))
shades <- c("blue", "green")
categories <- c("More humid tomorrow", "Less humid tomorrow" )
pie_labels <- paste0(categories, " = ", round(100*(pct/nrow(WAUS)),2), "%")
pie(pct, labels = pie_labels, col = shades)

# Selecting real-valued attributes
real_attributes <- WAUS[, c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine",
                            "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm", "Pressure9am",
                            "Pressure3pm", "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm",
                            "RISK_MM")]
summary(real_attributes) # Mean, median, etc
apply(real_attributes,2,sd) #standard deviations

################################################################################
# Q2. Pre-processing contd.: 
# Handling the outliers:
boxplot( WAUS$WindGustSpeed, as.integer(WAUS$Rainfall), WAUS$RISK_MM, names=c("WindGustSpeed","Rainfall","RISK_MM") )

# Remove outliers for WindGustSpeed (IQR Method)
quartiles <- quantile(WAUS$WindGustSpeed,probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(WAUS$WindGustSpeed)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
WAUS <- subset(WAUS, WAUS$WindGustSpeed > Lower & WAUS$WindGustSpeed < Upper)

# Remove outliers for Rainfall and RISK_MM based on the boxplot
WAUS <- subset(WAUS, WAUS$Rainfall<40) 
WAUS <- subset(WAUS,WAUS$RISK_MM<35) 

# Encode data
WAUS$RainToday <-ifelse(WAUS$RainToday=="Yes",1, 0 )
WAUS$WindDir3pm <- as.numeric(factor(WAUS$WindDir3pm))
WAUS$WindGustDir <- as.numeric(factor(WAUS$WindGustDir))
WAUS$WindDir9am <- as.numeric(factor(WAUS$WindDir9am))
WAUS$MHT <- as.factor(WAUS$MHT)

################################################################################
# Q3: Divide data into a 70% training and 30% test set
set.seed(30806151)  #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

################################################################################
# Q4: Implement a classification model using each of the following techniques
# Decision Tree
decTreeModel = tree(MHT~., data = WAUS.train)

# Naïve Bayes
naiveBayesModel = naiveBayes(MHT~., data = WAUS.train)

# Bagging
baggingModel <- bagging(MHT~., data = WAUS.train, mfinal = 10)

# Boosting
boostingModel <- boosting(MHT~., data = WAUS.train, mfinal = 10)

# Random Forest
randomForestModel <- randomForest(MHT~., data = WAUS.train)

################################################################################
# Q5: Using the test data, classify each of the test cases as ‘more humid tomorrow’ or ‘less humid tomorrow’. # Create a confusion matrix and report the accuracy of each model.

# Decision Tree
decTreePredict = predict(decTreeModel, WAUS.test, type = "class" )
table(observed = WAUS.test$MHT, predicted = decTreePredict)

# Naïve Bayes
naiveBayesPredict = predict(naiveBayesModel, WAUS.test)
table(observed = WAUS.test$MHT, predicted = naiveBayesPredict)

# Bagging
baggingPredict <- predict.bagging(baggingModel, WAUS.test)
cat("\n#Bagging Confusion\n")
print(baggingPredict$confusion)

# Boosting
boostingPredict <- predict.boosting(boostingModel, WAUS.test)
cat("\n#Boosting Confusion\n")
print(boostingPredict$confusion)

# Random Forest
randomForestPredict <- predict(randomForestModel, WAUS.test)
table(observed = WAUS.test$MHT, predicted = randomForestPredict)

################################################################################
# Q6: Using the test data, calculate the confidence of predicting ‘more humid tomorrow’ for
# each case

# Decision Tree
decTreePredictV = predict(decTreeModel, WAUS.test, type = "vector" )
decTreePredictV[,2]
WAUS.test$MHT
DCTpred = prediction(decTreePredictV[,2], WAUS.test$MHT)
DCTperf <-performance(DCTpred, "tpr", "fpr")
plot(DCTperf, col = 'red')
abline(0,1)

DCTauc = performance(DCTpred, "auc") # Calculate the AUC 
print(as.numeric(DCTauc@y.values))

# Naïve Bayes
naiveBayesPredictV = predict(naiveBayesModel, WAUS.test, type = "raw")
NBpred <- prediction(naiveBayesPredictV[,2], WAUS.test$MHT)
NBperf <- performance(NBpred, "tpr", "fpr")
plot(NBperf, add = TRUE, col = 'pink')

NBauc = performance(NBpred, "auc") # Calculate the AUC 
print(as.numeric(NBauc@y.values))

# Bagging
bagPred <- prediction(baggingPredict$prob[,2], WAUS.test$MHT)
bagPerf <- performance(bagPred, "tpr", "fpr")
plot(bagPerf, add = TRUE, col = 'green')

bagAuc = performance(bagPred, "auc") # Calculate the AUC 
print(as.numeric(bagAuc@y.values))

# Boosting
boostPred <- prediction(boostingPredict$prob[,2], WAUS.test$MHT)
boostPerf <- performance(boostPred, "tpr", "fpr")
plot(boostPerf, add = TRUE, col = 'blue')

boostAuc = performance(boostPred, "auc") # Calculate the AUC 
print(as.numeric(boostAuc@y.values))

# Random Forest
randomForestPredictV <- predict(randomForestModel, WAUS.test, type = "prob")
RFpred <- prediction(randomForestPredictV[,2], WAUS.test$MHT)
RFperf <- performance(RFpred, "tpr", "fpr")
plot(RFperf, add = TRUE, col = 'orange')

RFauc = performance(RFpred, "auc") # Calculate the AUC 
print(as.numeric(RFauc@y.values))

# Creating a legened for ROC
legend( x = "bottomright", legend = c("Decision Tree","Naïve Bayes","Bagging","Boosting","Random Forest"), fill = c("red","pink","green","blue","orange"), cex=0.6)

################################################################################
# Q8:Examining each of the models, determine the most important variables in predicting whether it will be more humid tomorrow or not

cat("\n#Decision Tree Attribute Importance\n")
print(summary(decTreeModel))

cat("\n#Baging Attribute Importance\n")
print(baggingModel$importance)

cat("\n#Boosting Attribute Importance\n")
print(boostingModel$importance)

cat("\n#Random Forest Attribute Importance\n")
print(randomForestModel$importance)

################################################################################
# Q9: Starting with one of the Q4 classifiers, create a classifier that is simple enough for a person to be able to classify whether it will be more humid tomorrow or not by hand.
simpleModel=tree(MHT~ WindSpeed9am + Rainfall + Pressure9am + RISK_MM + Temp3pm + WindGustSpeed + Evaporation + MinTemp + WindDir9am + Sunshine + Cloud3pm + Location + WindSpeed3pm + Pressure3pm, data = WAUS.train)
# Cross Validation
cvModel = cv.tree(simpleModel, FUN = prune.misclass) 
print(cvModel)
# Post-pruning
pruneModel = prune.misclass(simpleModel, best = 10)
plot(pruneModel)
text(pruneModel, pretty = 0)
#Building confusion matrix to determine accuracy
simplePredict = predict(pruneModel, WAUS.test, type = "class" )
table(observed = WAUS.test$MHT, predicted = simplePredict)

# Testing and determining the AUC value
simpleV = predict(pruneModel, WAUS.test, type = "vector" )
simplepred <- prediction(simpleV[,2], WAUS.test$MHT)
simplepref <-performance(simplepred, "tpr", "fpr")
simpleAUC = performance(simplepred, "auc") # Calculate the AUC 
print(as.numeric(simpleAUC@y.values))

################################################################################
# Q10: Creating the best tree-based classifier
#Approach 1: Cross validation method
WAUS.train <- data.frame(WAUS.train)
ctrl <- trainControl(method = "cv", number = 10)

model <- train(MHT~., data = WAUS.train, method = "cforest", trControl = ctrl)
prediction <- predict(model, WAUS.test)
confusion_matrix <- table(prediction, WAUS.test$MHT)
confusion_matrix

V <- predict(model, WAUS.test, type = "prob")
pred <- prediction(V[,2], WAUS.test$MHT)
perf <- performance(pred, "tpr", "fpr")
auc = performance(pred, "auc") # Calculate the AUC 
print(as.numeric(auc@y.values))

#Approach 2: Adjusting parameters
bestTBCModel <- randomForest(MHT~ .- RainToday , data = WAUS.train, ntree = 1000)
bestTBCPredict <- predict(bestTBCModel, WAUS.test)
bestTBCPredict
# Confusion matrix to calculate accuracy
table(observed = WAUS.test$MHT, predicted = bestTBCPredict)

# Calculating AUC
bestTBCPredictV <- predict(bestTBCModel, WAUS.test, type = "prob")

bestTBCpred <- prediction(bestTBCPredictV[,2], WAUS.test$MHT)
bestTBCperf <- performance(bestTBCpred, "tpr", "fpr")

bestTBCauc = performance(bestTBCpred, "auc") # Calculate the AUC 
print(as.numeric(bestTBCauc@y.values))

################################################################################
# Q11: Implement an Artificial Neural Network classifier
library(neuralnet) 

# Clearing environment except for some variables
rm(list= ls()[!(ls() %in% c('WAUS','WAUS.train', 'WAUS.test'))]) 

# Making a copy of both sets to use for nn
nnWAUS.train = WAUS.train
nnWAUS.test = WAUS.test

# Adding separate columns for more humid tomorow: if 1 then More, if 0 then Les
nnWAUS.train$More=nnWAUS.train$MHT == 1
nnWAUS.train$Less=nnWAUS.train$MHT == 0

# Data pre-processing: standardization: all values have mean of 0 and standard deviation of 1
preproc <- preProcess(nnWAUS.train, method = c("center", "scale"))  # Adjust input_columns as per your dataset
nnWAUS.train <- predict(preproc, nnWAUS.train)
nnWAUS.test <- predict(preproc, nnWAUS.test)

# Building the ANN model
waus.nn = neuralnet(More + Less~ .- RainToday - Year - MHT, nnWAUS.train, hidden = 4, rep = 2)
plot(waus.nn, rep = "best") #plotting
waus.nn$result.matrix

# Predicting
wausnn.pred = compute(waus.nn, nnWAUS.test[,!names(nnWAUS.test) %in% c("RainToday", "Year", "MHT")])
wausnn.predr = round(wausnn.pred$net.result, 0) #Rounding result
wausnn.predrdf = as.data.frame(as.table(wausnn.predr)) #Converting to a dataframe
# Leave only ones
wausnn.predrdfs = wausnn.predrdf[!wausnn.predrdf$Freq == 0,] 
wausnn.predrdfs = wausnn.predrdfs[!wausnn.predrdfs$Freq == -1,] 

wausnn.predrdfs
# Changing the format to get the confusion matrix
wausnn.predrdfs$Freq=NULL

wausnn.predrdfs$Var2 <-ifelse(wausnn.predrdfs$Var2=="A",1, 0 )

colnames(wausnn.predrdfs) = c("Obs", "MHT")
wausnn.predrdfs = wausnn.predrdfs[order(wausnn.predrdfs$Obs),]

#Getting confusion matrix
table(observed = nnWAUS.test$MHT, predicted = wausnn.predrdfs$MHT)

# Calculate AUC value
auc_value <- roc(nnWAUS.test$MHT, wausnn.predrdfs$MHT)$auc

auc_value

################################################################################
# Q12: Fit a new classifier to the data, test and report its performance in the same way as for previous models. 
rm(list= ls()[!(ls() %in% c('WAUS','WAUS.train', 'WAUS.test'))]) 

require(xgboost)

#define predictor and response variables in training set
train_x = data.matrix(WAUS.train[,!names(WAUS.train) %in% c("MHT")])
train_y = WAUS.train$MHT

#define predictor and response variables in testing set
test_x = data.matrix(WAUS.test[,!names(WAUS.test) %in% c("MHT")])
test_y = WAUS.test$MHT

# Converting to numerical 0 and 1 from factor
train_y <- ifelse(as.numeric(train_y) == "1", 0, 1)
test_y <- ifelse(as.numeric(test_y) == "1", 0, 1)

#define final training and testing sets (converting to a proper matrix format)
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Set the parameters for the XGBoost model
params <- list(
  objective = "binary:logistic", #binary classification using logistic regression
  eval_metric = "logloss" # evaluation metric 
)

# Train the XGBoost model with 100 boosting rounds
xgb_model <- xgboost(params = params, data = xgb_train, nrounds = 50)

# Predict on the test set using the trained model
predictions <- round((predict(xgb_model, xgb_test)),0)

# Getting a confusion matrix
table(observed = WAUS.test$MHT, predicted = predictions)

# Calculating the AUC value
pred <- prediction(predictions, WAUS.test$MHT)
boostAuc = performance(pred, "auc")  
print(as.numeric(boostAuc@y.values))
