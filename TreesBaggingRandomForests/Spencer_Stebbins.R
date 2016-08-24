#--------------------------------------------------
# Question #1: Trees
#--------------------------------------------------
# Load the  OJdataset from the  ISLRlibrary into your workspace. The data contains 1,070 purchases where the customer either purchased Citrus Hill or Minute Maid orange juice. A number of characteristics of the customer and product are recorded.
library(ISLR)
data(OJ)

# 1. Split the data into a training and test set with an 80% - 20% split, respectively. (N B: Use  set.seed(0) so your results will be reproducible.)
#Splitting the data into training and test sets by an 70% - 30% split.
set.seed(0)
train = sample(1:nrow(OJ), 8*nrow(OJ)/10) #Training indices.
OJ.train = OJ[train,]
OJ.test = OJ[-train, ] #Test dataset.
Purchase.test = Purchase[-train] #Test response.


# 2. Construct an initial decision tree predicting  Purchasefrom all other variables in the training dataset defining splits based upon the Gini coefficient.
library(tree)
tree.OJ = tree(Purchase ~ ., split = "gini", data = OJ.train)
summary(tree.OJ)
plot(tree.OJ)
text(tree.OJ, pretty = 0)
summary(tree.OJ)
# Classification tree:
#     tree(formula = Purchase ~ ., data = OJ.train, split = "gini")
# Variables actually used in tree construction:
#     [1] "SpecialMM"      "SpecialCH"      "DiscCH"         "DiscMM"         "LoyalCH"       
# [6] "STORE"          "WeekofPurchase" "PriceMM"        "StoreID"        "PriceCH"       
# [11] "PriceDiff"      "Store7"         "SalePriceMM"    "ListPriceDiff"  "PctDiscMM"     
# Number of terminal nodes:  86 
# Residual mean deviance:  0.6052 = 466 / 770 
# Misclassification error rate: 0.1449 = 124 / 856 

# 3. How many terminal nodes are there in your initial tree? What is the accuracy of your initial tree?
# Number of terminal nodes:  86
# Residual mean deviance:  0.6052 = 466 / 770 

# 4. Predict the  Purchasevariable for observations that are within your test set using this initial tree. Report the accuracy of your predictions.
tree.pred = predict(tree.OJ, OJ.test, type = "class")
table(tree.pred, OJ.test$Purchase)
(103 + 60)/nrow(OJ.test)
# 0.7616822

# 5. Implement cross-validation and, thus, cost-complexity pruning to determine how far back to prune your tree. (N B: Use  set.seed(0)so your results will be reproducible.)
set.seed(0)
OJ.cv = cv.tree(tree.OJ, FUN = prune.misclass)

# 6. Visualize your results from part 5 across various numbers of terminal nodes/values for alpha.
plot(OJ.cv$size, OJ.cv$dev, type = "b", xlab = "Terminal Nodes", ylab = "Misclassified Observations")
plot(OJ.cv$k, OJ.cv$dev, type  = "b",xlab = "Alpha", ylab = "Misclassified Observations")

# 7. Prune your tree based on the results of part 6.
prune.OJ = prune.misclass(tree.OJ, best = 4)

# 8. How many terminal nodes are there in your pruned tree? What is the accuracy of
# your pruned tree?
summary(prune.OJ)
# Number of terminal nodes:  6 

# 9. Visualize your pruned tree.
plot(prune.OJ)
text(prune.OJ, pretty = 0)

# 10. Predict the  Purchasevariable for observations that are within your test set using
# this pruned tree. Report the accuracy of your predictions.
tree.pred = predict(prune.OJ, OJ.test, type = "class")
table(tree.pred, OJ.test$Purchase)
(109 + 53)/nrow(OJ.test)
# 0.7570093

# 11. Why are the test set predictions more accurate for the pruned tree than those for
# the initial tree?
#initial tree, with its large number of terminal nodes was overfit to the train sample

#--------------------------------------------------
# Question #2: Bagging & Random Forests
#--------------------------------------------------
# Continue using the  OJdataset and the training/test sets you already loaded into your workspace.
# 1. Construct an initial random forest predicting  Purchasefrom all other variables in the training dataset using the default settings; this will create 500 trees. (N B: Use set.seed(0) so your results will be reproducible.)
library(randomForest)
set.seed(0)
rf.OJ = randomForest(Purchase ~ ., data = OJ, subset = train, importance = TRUE)

# 2. What is the accuracy of this initial random forest on:
        
#     a. The training set?
        # randomForest(formula = Purchase ~ ., data = OJ, importance = TRUE,      subset = train) 
        # Type of random forest: classification
        # Number of trees: 500
        # No. of variables tried at each split: 4
        # 
        # OOB estimate of  error rate: 19.86%
        # Confusion matrix:
        #     CH  MM class.error
        # CH 447  75   0.1436782
        # MM  95 239   0.2844311
        #19.86%

    # b. The test set?
    rf.OJ.pred=predict(rf.OJ,OJ.test)
    table(rf.OJ.pred,OJ.test$Purchase)
    (113+60)/214
    # 0.8084112
        
# 3. Which variable is aiding the most in classifying the orange juice purchases?
importance(rf.OJ)
varImpPlot(rf.OJ)  
#LoyalCH
    
# 4. Vary the number of variables considered as candidates at each node split in the
# random forest procedure (from one to all predictors). Record the out-of-bag error rates for each of these random forests on the training set. (N B: Use  set.seed(0) so your results will be reproducible.) (H int: You will want to record the error rate instead of the MSE since this is a classification problem. If you are modifying class code, try using the code snippet  fit$err.rate[500,1]. )
set.seed(0)
oob.err = numeric(17)
for (mtry in 1:17) {
    fit = randomForest(Purchase ~ ., data = OJ[train, ], mtry = mtry)
    oob.err[mtry] = fit$err.rate[500,1]
    cat("We're performing iteration", mtry, "\n")
}

# 5. Visualize the out-of-bag error rates as they change with the number of variables considered at each node split.
#Visualizing the OOB error.
plot(1:17, oob.err, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

# 6. What is the maximum accuracy among your random forests on the training set? How many variables were considered at each split in this best random forest?
#maximum accuracy is a little more than 81%.  Only 2 splits considered here.

# 7. What is the accuracy of the bagged model on the training set? How many variables
# were considered at each split in this bagged model?
#Bagged accuracy is 1-.2033 = 79.67%.  It was using all 17 variables for each tree.

# 8. What is the accuracy of the best random forest from part 6 on the test set? (N B:
#                                                                                     Use  set.seed(0) so your results will be reproducible.)
set.seed(0)
OJ.best.fit = randomForest(Purchase ~ ., data = OJ.train, mtry = 2)
OJ.rf.pred2=predict(OJ.best.fit,OJ.test)
table(OJ.rf.pred2,OJ.test$Purchase)
(118+58)/214
#Accuracy on the test set is 82.24%

# 9. What is the accuracy of the bagged model on the test set? (N B: Use  set.seed(0)
#                                                              so your results will be reproducible.)
set.seed(0)
OJ.bag=randomForest(Purchase ~.,data=OJ.train,mtry=17)
OJ.bag.pred=predict(OJ.bag,OJ.test)
table(OJ.bag.pred,OJ.test$Purchase)
(110+62)/214
#Bagged model accuracy is 80.37%


#--------------------------------------------------
# Question #3: Boosting
#--------------------------------------------------
# Continue using the  OJdataset and the training/test sets you already loaded into your workspace.
# 1. In order to boost with classification trees, we need to do a bit of data munging to transform the response variable. You may use the following lines of code to produce the copies of your dataset  OJ.train.indicator and OJ.test.indicatorthat have a transformed response variable. (N B: You must replace  OJ.trainand  OJ.testwith whatever names you used in your own code.)
OJ.train.indicator = OJ.train 
OJ.test.indicator = OJ.test
OJ.train.indicator$Purchase = as.vector(OJ.train$Purchase, mode = "numeric") - 1
OJ.test.indicator$Purchase = as.vector(OJ.test$Purchase, mode = "numeric") - 1

# 2. Construct an initial boosted model on the training set that uses all of the following settings at once: (N B: Use  set.seed(0) so your results will be reproducible.)
    # a. The Bernoulli distribution.
    # b. 10,000 trees.
    # c. An interaction depth of 4.
    # d. A shrinkage parameter of 0.001.
library(gbm)
set.seed(0)
OJ.boost = gbm(Purchase ~ ., data = OJ.train.indicator,
               distribution = 'bernoulli',
               n.trees = 10000,
               interaction.depth = 4,
               shrinkage = 0.001)

# 3. Predict your test set observations using the initial boosted model across up to 10,000 trees, considering groups of 100 trees at a time. (H int: Use  type = "response") and round your ultimate predictions.)
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = round(predict(OJ.boost, newdata = OJ.test.indicator, n.trees = n.trees,type='response'))
OJ.boost.match = predmat==OJ.test.indicator$Purchase

# 4. Calculate and store the accuracy for each of the 100 models considered in part 3. What is the minimum number of trees required to reach the maximum accuracy?
OJ.acc = NULL
for (i in 1:100){
    OJ.acc[i]=sum(OJ.boost.match[,i])/214
}
plot(OJ.acc)
OJ.acc[20:30]
#2,100 trees needed to achieve maximum accuracy

# 5. Plot the accuracies found in part 4 against the number of trees. Add to the plot:
    # a. A horizontal line marking the best boosted accuracy on the test set.
    # b. A horizontal line marking the best random forest accuracy on the test set.
    # c. A horizontal line marking the best pruned decision tree accuracy on the test set.
plot(OJ.acc, type='b')
abline(h=max(OJ.acc),col='red')
abline(h=.8224,col='blue')
abline(h=((114+57)/214),col='green')

