######################
###  Main Function ###
######################


#############################
####  Preprocessing data ####
#############################

# Read data and mark 999.0 as NAs
dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA
testId = dfTest$EventId

str(dfTrain)

weight <- dfTrain$Weight
labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

sum(complete.cases(train)) # 68114

######## Only about a quarter of the dataset are complete cases, so how do we impute those missing values?
######## Most tree models can handle missing values automatically. Check the following links to see how random forest and gbm
######## handle missing values in the training set.
######## http://stackoverflow.com/questions/14718648/r-gbm-handling-of-missing-values
######## http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#missing1


##########################
####  Build gbm model ####
##########################

library(caret)
###### Check the documentation for the details of those functions
###### https://cran.r-project.org/web/packages/caret/caret.pdf

####### Library used for parallel processing
####### Check details here: http://topepo.github.io/caret/parallel.html
####### Windows users please follow the instruction on stackoverflow: http://stackoverflow.com/a/24655923
library(doMC)
registerDoMC(cores = 4)

# Load our customized metric function.
source('helper.R')

###### Setup a 5 fold cross-validation and use AMS as the metric
###### AMS_summary function defined in helper.R
###### Check details here: http://topepo.github.io/caret/training.html#control
ctrl = trainControl(method = "repeatedcv",number = 2,
                    summaryFunction = AMS_summary)

###### Setup grid search parameters. 
###### The more candidates you provide, the longer it will take to train the model.
gbmGrid <-  expand.grid(interaction.depth = c(4,6,8), n.trees =(2:8)*100,
                        shrinkage = c(0.01, 0.005, 0.001),
                        n.minobsinnode = c(100, 500, 2000))

###### Train the gbm model
###### If you want to use the gbmGrid you defined above, you could simply set tuneGrid = gbmGrid in the train function.
m_gbm = train(x=train, y=labels, 
              method="gbm", weights=weight, 
              verbose=TRUE, trControl=ctrl, metric="AMS")

###### You can think of this model as a logistic regression. For a logistic regression, we need to find the best threshold for ROC and AUC.
###### Check the definition of ROC and AUC here: 
###### http://thestatsgeek.com/2014/05/05/area-under-the-roc-curve-assessing-discrimination-in-logistic-regression/
gbmTrainPred <- predict(m_gbm, newdata=train, type="prob")

library(pROC)
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, gbmTrainPred[,2])
plot(auc, print.thres=TRUE)

######## From the graph, we can tell the best threshold is 0.002
threshold <- 0.002

gbmTestPred <- predict(m_gbm, newdata=test, type="prob")

predicted <- rep("b",550000)
predicted[gbmTestPred[,2]>=threshold] <- "s"
weightRank = rank(gbmTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "gbm_submission.csv", row.names=FALSE)

####################################
####  Build random forest model ####
####################################

###### The only thing you need to change is the name of method.
###### Check all the available algorithms by typing names(getModelInfo())
###### Check avaliable tuning parameters here: http://topepo.github.io/caret/modelList.html
rfGrid <-  expand.grid(mtry = c(3,6,9))

m_rf = train(x=train, y=labels, 
             method="rf", weights=weight, 
             verbose=TRUE, trControl=ctrl, metric="AMS")


######################
#####   Xgboost  #####
######################

###### Check the source code here: https://github.com/dmlc/xgboost/tree/master/demo/kaggle-higgs
###### The slides I found quite useful to understand xgboost: https://homes.cs.washington.edu/~tqchen/pdf/BoostedTree.pdf
###### The offical paper of xgboost: https://arxiv.org/pdf/1603.02754v2.pdf



####################################
#####   Where to go from here  #####
####################################


######## 1. Feature engineering
######## It is a huge topic but you can get some idea from the winning solution: https://github.com/phunterlau/kaggle_higgs


######## 2. Missing values
######## Tree models like random forest or gbm could handle missing values automatically. So does xgboost.
######## What if you just want to build a simple logistic regression?

######## 3. Ensemble method
######## How to make you final model works better by ensembling couple models?
######## For a classification problem, a majority vote would be a straight-forward approach.
######## Check out this tutorial for a complete guide: http://mlwave.com/kaggle-ensembling-guide/


######## 4. Stacking method
######## This is a more advanced topic. Most of the winning solutions on Kaggle using two-layer stacking method.
######## Check out this tutorial http://machinelearningmastery.com/machine-learning-ensembles-with-r/

