#-----------------------------------
# Question #1: Wine Quality
#-----------------------------------

# Read in the  [10] Wine Quality.csvdataset into your workspace. The data contains 1,599 observations of red Vinho Verde wines from the north of Portugal. The goal is to
# model of wine quality based on various physicochemical measurements.
data <- read.csv("~/Documents/NYCDSA/Course Work/SupportVectorMachines/Support Vector Machines Homework/[10] Wine Quality.csv")
# 1. Perform some data munging:
#     a. Recode the  qualityvariable to be a factor variable with values of “Low”
    # for quality ratings of 5 and below, and “High” for ratings of 6 and above.
    data$quality <- as.factor(sapply(data$quality, function(x) { ifelse(x >= 6, 'High', 'Low')}))
    
    # b. Scale and center the numeric vectors of your dataset.
    data[,-12] = as.data.frame(scale(data[,-12]))
    
# 2. Split the data into a training and test set with an 80% - 20% split, respectively. (N B: Use  set.seed(0) so your results will be reproducible.)
set.seed(0)
data <- as.dataframe(data)
train = sample(1:nrow(data),0.8*nrow(data))
wine.train <- data[train,]
wine.test  <- data[-train,]
    
# 3. Briefly explore some graphical EDA:
color <- ifelse(data$quality=='Low','blue','red')
plot(data[1:11], col= color)

#     a. Explain why a maximal margin classifier is impossible to fit to this data.
# (N B: Do not try to fit the maximal margin classifier.)
    # Because the data cannot be linearly sliced on almost any dimension. 
        
    # b. Explain why a support vector classifier is generally more desirable.
    # Similar as to stated above, a support vector classifier is more desirable because it can provide some slack and the data
    # doesnt have to be sliced just linearly

# 4. Tune a support vector classifier with a cost ranging from 10 to 10 ; try using the
# code snippet  cost = 10^(seq(�5, �.5, length = 50)). C aution: This will take about a minute to run. (N B: Use  set.seed(0)so your results will be reproducible.)
library(e1071)
set.seed(0)
wine.train$quality <- ifelse(wine.train$quality=='Low',-1,1)
cv.svm.wine <- tune(svm, quality ~ ., data = wine.train,  kernel = "linear", ranges = list(cost = 10^(seq(-5, -.5, length = 50))))

    # a. What was the best cost parameter of the ones you tested?
    summary(cv.svm.wine)
    # Parameter tuning of ‘svm’:
    #     
    #     - sampling method: 10-fold cross validation 
    # 
    # - best parameters:
    #     cost
    # 0.0008483429
    # 
    # - best performance: 0.7327626 

    # b. What was the best error rate corresponding to the best cost?
    par(mfrow=c(1,1))
    plot(cv.svm.wine$performances$cost,
         cv.svm.wine$performances$error,
         xlab = "Cost",
         ylab = "Error Rate",
         type = "l")
    #The plot shows that we spend very little cost on the part that reduces the error the most.
    #If we were to narrow down to these features causing the most error, we coudl decreae cost
    
    # c. Graphically view the cross-validated results. Is it plausible that you checked
    # enough values of cost?
    # because it flattens out so much, the assumption is we checked enough values

# 5. How many support vectors are there in your best support vector classifier?
svm.wine.best  = svm(quality ~ ., data = wine.train,kernel = "linear", cost = 0.0008483429)
summary(svm.wine.best)
# Number of Support Vectors:  1198

# 6. What is the test error associated with the best support vector classifier you found
# in part 4?
wine.test$quality<-ifelse(wine.test$quality=='Low',-1,1)
ypred <- predict(cv.svm.wine$best.model, wine.test[,-12])
ypred2<-ifelse(ypred<0,-1,1)
table("Predicted Values" = ypred2, "True Values" = wine.test[,'quality'])
(108+120)/nrow(wine.test)
# 0.7125

# 7. Fit a support vector classifier to all of the data using the best cost parameter you found in part 4.
data$quality <- ifelse(data$quality=='Low',-1,1)
data.overall<-svm(quality ~ .,
                  data = data,
                  kernel = "linear",
                  cost = 0.0008483429)

summary(data.overall)
# Parameters:
#     SVM-Type:  eps-regression 
# SVM-Kernel:  linear 
# cost:  0.0008483429 
# gamma:  0.09090909 
# epsilon:  0.1 
# 
# 
# Number of Support Vectors:  1502
    # a. How many support vectors does this support vector classifier have?
    #1502

    # b. Is the 555th observation a support vector?
    sum(data.overall$index==555)
    # 1 so yes
    
# 8. What is the overall error rate for the support vector classifier you created in part 7?
names(data.overall)
classification<-ifelse(data.overall$decision.values<0,-1,1)
table('classified'=classification,'true value'=data$quality)
# true value
# classified  -1   1
# -1 619 329
# 1  125 526    
(606+526)/nrow(data)
# 0.7079425

# 9. Visualize the support vector classifier by examining the free sulfur dioxide and
# total sulfur dioxide cross-section; to do so, use the following line of code (modified with your object names):  plot(model, data, free.sulfur.dioxide ~ total.sulfur.dioxide). 
plot(data.overall, data, free.sulfur.dioxide ~ total.sulfur.dioxide)

# 10. Tune a support vector machine with a radial kernel. Check both cost and gamma values using the following code snippets:  cost = seq(.75, 1.25, length = 5), gamma = seq(.55, .95, length = 5). C aution: This will take about a minute to run. (N B: Use  set.seed(0) so your results will be reproducible.)
set.seed(0)
cv.svm.radial = tune(svm, quality ~ ., data = wine.train, kernel = "radial", ranges = list( cost = seq(.75, 1.25, length = 5),  gamma = seq(.55, .95, length = 5)))
summary(cv.svm.radial)
# - best parameters:
#     cost gamma
# 1.25  0.75
# 
# - best performance: 0.6298074 

    # a. What was the best cost parameter of the ones you tested?
    # - best performance: 0.6298074 
    
    # b. What was the best gamma parameter of the ones you tested?
    # 0.75
    
    # c. What was the best error rate corresponding to the best cost & gamma?
    # ~.38%
    
    # d. Graphically view the cross-validated results. Is it plausible that you checked
    # enough values of cost and gamma?
    plot(cv.svm.radial$performances$cost,
         cv.svm.radial$performances$error,
         xlab = "Cost",
         ylab = "Error Rate",
         type = "l")
    # Based on this grpah, it seems that maybe the error rate will taper off further with additional variables being checked

# 11. How many support vectors are there in your best support vector machine?
cv.svm.radial$best.model
# Number of Support Vectors:  1113
  
        
# 12. What is the test error associated with the best support vector machine you found
# in part 10?
pred <- predict(cv.svm.radial$best.model, wine.test[,-12])
pred2<-ifelse(pred<0,-1,1)
table("Predicted Values" = pred2, "True Values" = wine.test[,'quality'])
# True Values
# Predicted Values  -1   1
# -1 102  35
# 1   40 143
(102 + 143)/nrow(wine.test)
# 13. Fit a support vector machine to all of the data using the best cost and gamma
# parameters you found in part 10.
wine.svm.radial <- svm(quality ~ .,data=data,kernel='radial', cost=1.25, gamma=0.75)

    # a. How many support vectors does this support vector machine have?
    wine.svm.radial$best.model
    # b. Is the 798th observation a support vector?
    sum(wine.svm.radial$index==798)

# 14. What is the overall error rate for the support vector machine you created in part 13?
classification<-ifelse(svm.wine.radial$decision.values<0,-1,1)
table('classified'=classification,'true value'=data$quality)
(37+45)/nrow(data)
# 0.05128205

# 15. Visualize the support vector machine by examining the free sulfur dioxide and total sulfur dioxide cross-section; to do so, use the following line of code (modified with your object names):  plot(model, data, free.sulfur.dioxide ~ total.sulfur.dioxide). 
plot(wine.svm.radial, data, free.sulfur.dioxide ~ total.sulfur.dioxide)

# 16. List a pro and con for both:
# a. The best support vector classifier you found in part 7.
# easier to interpret

# b. The best support vector machine you found in part 13.
# smaller error rate, but impossible to interpret