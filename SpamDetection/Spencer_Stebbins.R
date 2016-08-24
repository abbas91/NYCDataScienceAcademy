library(kernlab)
data(spam)
help(spam)

######################################################################
######################################################################
#####[09] Trees, Bagging, Random Forests, & Boosting Lecture Code#####
######################################################################
######################################################################

##############################
#####Classification Trees#####
##############################
#Loading the tree library for fitting classification and regression trees.
library(tree)

#Loading the ISLR library in order to use the Carseats dataset.
library(ISLR)

#Making data manipulation easier.
attach(spam)

#Looking at the variable of interest, type.
summary(type)
# nonspam    spam 
# 2788    1813 

complete.cases(spam)
na_count <-sapply(spam, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)
# no missing values

# create data frame
spam = data.frame(spam)


#Splitting the data into training and test sets by an 70% - 30% split.
set.seed(0)
train = sample(1:nrow(spam), 7*nrow(spam)/10) #Training indices.
spam.train = spam[train, ]
spam.test = spam[-train, ] #Test dataset.
type.test = type[-train] #Test response.

#Ftting and visualizing a classification tree to the training data.
tree.spam = tree(type ~ . - type, data = spam, subset = train)
plot(tree.spam)
text(tree.spam, pretty = 0)
summary(tree.spam)
# Classification tree:
#     tree(formula = type ~ . - type, data = spam, subset = train)
# Variables actually used in tree construction:
# [1] "charExclamation" "remove"          "money"           "george"         
# [5] "hp"              "capitalLong"     "our"             "charDollar"     
# [9] "capitalAve"      "free"            "your"           
# Number of terminal nodes:  14 
# Residual mean deviance:  0.4833 = 1549 / 3206 
# Misclassification error rate: 0.08758 = 282 / 3220 

#!! tree seleted 11 out of 57 variables

#Using the trained decision tree to classify the test data.
tree.pred = predict(tree.spam, spam.test, type = "class")
tree.pred

#Assessing the accuracy of the overall tree by constructing a confusion matrix.
table(tree.pred, type.test)
# tree.pred nonspam spam
# nonspam     770   93
# spam         64  454
(770 + 454)/nrow(spam.test)
# 0.8863143

#Performing cross-validation in order to decide how many splits to prune; using
#misclassification as the basis for pruning.
set.seed(0)
cv.spam = cv.tree(tree.spam, FUN = prune.misclass)

#Inspecting the elements of the cv.tree() object.
names(cv.spam)
cv.spam

#Size indicates the number of terminal nodes. Deviance is the criterion we
#specify; in this case it is the misclassification rate. K is analogous to the
#cost complexity tuning parameter alpha. Method indicates the specified criterion.

#Visually inspecting the results of the cross-validation by considering tree
#complexity.
par(mfrow = c(1, 2))
plot(cv.spam$size, cv.spam$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "Misclassified Observations")
plot(cv.spam$k, cv.spam$dev, type  = "b",
     xlab = "Alpha", ylab = "Misclassified Observations")


#Pruning the overall tree to have 4 terminal nodes; choose the best tree with
#4 terminal nodes based on cost complexity pruning.
par(mfrow = c(1, 1))
prune.spam = prune.misclass(tree.spam, best = 5)
plot(prune.spam)
text(prune.spam, pretty = 0)

#Assessing the accuracy of the pruned tree with 4 terminal nodes by constructing
#a confusion matrix.
tree.pred = predict(prune.spam, spam.test, type = "class")
table(tree.pred, type.test)
# tree.pred nonspam spam
# nonspam     790  145
# spam         44  402
(790 + 402)/nrow(spam.test)
# 0.8631427

#Originally we had 11 terminal nodes and an accuracy of 88.6%; now, there are
#only 5 terminal nodes with an accuracy of about 86.3%.


##################################
#####Bagging & Random Forests#####
##################################
library(randomForest)

#Fitting an initial random forest to the training subset.
set.seed(0)
rf.spam = randomForest(type ~ ., data = spam, subset = train, importance = TRUE)
rf.spam
# randomForest(formula = type ~ ., data = spam, importance = TRUE,      subset = train) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 7
# 
# OOB estimate of  error rate: 4.75%
# Confusion matrix:
#     nonspam spam class.error
# nonspam    1897   57  0.02917093
# spam         96 1170  0.07582938

#The MSE and percent variance explained are based on out-of-bag estimates,
#yielding unbiased error estimates. The model reports that mtry = 7, which is
#the number of variables randomly chosen at each split. Since we have 57 overall
#variables, we could try all 57 possible values of mtry. We will do so, record
#the results, and make a plot.

#Varying the number of variables used at each step of the random forest procedure.
set.seed(0)
oob.err = numeric(57)
for (mtry in 1:57) {
    fit = randomForest(type ~ ., data = spam[train, ], mtry = mtry)
    oob.err[mtry] = fit$err.rate[500]
    cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:57, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rf.spam)
varImpPlot(rf.spam)


##################
#####Boosting#####
##################
library(gbm)

#Fitting 10,000 trees with a depth of 4.
set.seed(0)
boost.spam = gbm(type ~ ., data = spam[train, ],
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4)

#Inspecting the relative influence.
par(mfrow = c(1, 1))
summary(boost.spam)
# charExclamation     charExclamation 2.359802e+01
# charDollar               charDollar 2.055702e+01
# remove                       remove 1.016790e+01
# hp                               hp 8.041604e+00
# free                           free 6.395318e+00
# capitalAve               capitalAve 6.226760e+00
# capitalLong             capitalLong 3.606880e+00
# edu                             edu 2.839594e+00
# your                           your 2.821260e+00
# george                       george 2.672052e+00
# capitalTotal           capitalTotal 2.399121e+00
# our                             our 2.027007e+00
# money                         money 1.839756e+00
# num000                       num000 6.665054e-01
# internet                   internet 6.559901e-01
# meeting                     meeting 5.932227e-01
# will                           will 5.334925e-01
# re                               re 4.263950e-01
# num1999                     num1999 4.062265e-01
# num650                       num650 3.928745e-01
# you                             you 3.385512e-01
# over                           over 3.228333e-01
# charSemicolon         charSemicolon 3.107901e-01
# business                   business 2.028163e-01
# report                       report 1.749031e-01
# email                         email 1.629161e-01
# receive                     receive 1.586534e-01
# charRoundbracket   charRoundbracket 1.458773e-01
# technology               technology 1.440994e-01
# project                     project 1.338695e-01
# hpl                             hpl 1.262996e-01
# font                           font 1.223609e-01
# mail                           mail 1.190018e-01
# people                       people 9.005265e-02
# pm                               pm 8.262899e-02
# make                           make 8.236036e-02
# conference               conference 7.768257e-02
# all                             all 6.377838e-02
# num3d                         num3d 5.963783e-02
# num85                         num85 5.740108e-02
# charHash                   charHash 5.339892e-02
# data                           data 3.367519e-02
# credit                       credit 1.223666e-02
# order                         order 1.223106e-02
# parts                         parts 1.097438e-02
# address                     address 9.991352e-03
# charSquarebracket charSquarebracket 8.233842e-03
# direct                       direct 6.054217e-03
# addresses                 addresses 4.736220e-03
# lab                             lab 2.689305e-03
# labs                           labs 1.478441e-03
# cs                               cs 4.178715e-04
# original                   original 1.894259e-04
# num415                       num415 1.704227e-04
# telnet                       telnet 0.000000e+00
# num857                       num857 0.000000e+00
# table                         table 0.000000e+00

#Partial dependence plots for specific variables; these plots illustrate the
#marginal effect of the selected variables on the response after integrating
#out the other variables.
par(mfrow = c(1, 2))
plot(boost.spam, i = "charExclamation")
plot(boost.spam, i = "charDollar")

#As the number fo exclamations and dollars signs increase in mail, the likelhood of that the mail being classified as spam
#increasing as well logarithmically

#Let’s make a prediction on the test set. With boosting, the number of trees is
#a tuning parameter; having too many can cause overfitting. In general, we should
#use cross validation to select the number of trees. Instead, we will compute the
#test error as a function of the number of trees and make a plot for illustrative
#purposes.
n.trees = seq(from = 10, to = 1000, by = 10)
predmat = round(predict(boost.spam, newdata = spam[-train, ], n.trees = n.trees))

#Produces 100 different predictions for each of the observations in our
#test set.
dim(predmat)

#Calculating the boosted errors.
spam.test.indicator = spam.test
spam.train.indicator = spam.train
spam.test.indicator$type = as.vector(spam.test$type, mode = "numeric") - 1
spam.train.indicator$type = as.vector(spam.train$type, mode = "numeric") - 1

par(mfrow = c(1, 1))
boost.err = with(spam.test.indicator, apply((predmat - spam.test.indicator$type)^2, 2, mean))
plot(n.trees, boost.err, pch = 16,
     ylab = "Classification Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

min(boost.err)
# 0.6039102
#Include the min boost.err to determine optimal trees
abline(h = min(min(boost.err)), col = "red")
which.min(boost.err)
boost.err[(boost.err >= .6039)]
# 10 trees
#Increasing the shrinkage parameter; a higher proportion of the errors are
#carried over.
set.seed(0)
boost.spam2 = gbm(type ~ ., data = spam[train, ],
                    distribution = "gaussian",
                    n.trees = 1000,
                    interaction.depth = 4,
                    shrinkage = 0.01)
predmat2 = predict(boost.spam2, newdata = spam[-train, ], n.trees = n.trees)

boost.err2 = with(spam.test.indicator, apply((predmat2 - spam.test.indicator$type)^2, 2, mean))
plot(n.trees, boost.err2, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

min(boost.err2)
# 1.051183
boost.err2[(boost.err2 <= 1.051183)]
abline(h = min(min(boost.err2)), col = "red")
# 780
