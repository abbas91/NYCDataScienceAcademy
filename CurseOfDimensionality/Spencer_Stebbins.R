# ------------------------------------------>
# Question #1: Principal Component Analysis
# ------------------------------------------>
# Load the  heptathlon dataset from the  HSAUR library into your workspace. A heptathlon is a combined track and field event-based contest for women. This dataset contains scores on each event for the 1988 olympic heptathlon competition held in Seoul.
library(HSAUR) 
data <- heptathlon

principal() #Performs principal components analysis with optional rotation.
factor.plot() #Visualizes the principal component loadings.
bodies = Harman23.cor$cov #Covariance matrix of 8 physical measurements on 305 girls.
bodies
# 1. Create a scatterplot matrix of all variables in the dataset. Briefly comment on the nature of the data.
plot(data)
# There appears to be linearity between many of the features. For instance, score and hurdles
# seem to have a strong negative correlation whereas longjump and score have a strong 
# positive correlation

# 2. It will help to have all event scores going in the “same direction” (i.e., a higher event score implies a better performance, and a lower event score implies a worse performance). To do so, transform the hurdle and running variables by subtracting the original scores for each heptathlete from the maximum score of each of those variables, respectively.
x<-data
x$Mhurdles<-max(x$hurdles)-x$hurdles
x$Mrun200m<-max(x$run200m)-x$run200m
x$Mrun800m<-max(x$run800m)-x$run800m
x$run800m<-NULL
x$run200m<-NULL
x$hurdles<-NULL

# 3. Create a scatterplot matrix of all the event score variables in the “same direction.” Briefly comment on the nature of the data.
# After accounting for the revertion of the hurdles and runnings, the negative correlations talked baout in 
# question 1 are converted into positive to match the other data

# 4. Create a scree-plot of your newly created dataset that doesn’t include the  score variable. From this plot, describe how you determine the number of principal components to extract in three different ways.
library(psych) 
y<-x
y$score<-NULL
fa.parallel(y[,1:7],fa='pc')
# 1) The first compoenent is the only eigen value above 1
# 2) The first componenet is the only eigen value above the PC cimulated line with random sampling
# 3) The second component appears to be where the elbow lies and the following component values have decreasing effect

# 5. Extract the appropriate number of principal components from your dataset that does not include the  score  variable and save this object.
y_principal <-principal(y)
# PC1    h2    u2 com
# highjump 0.80 0.635 0.365   1
# shot     0.77 0.588 0.412   1
# longjump 0.96 0.928 0.072   1
# javelin  0.16 0.025 0.975   1
# Mhurdles 0.96 0.915 0.085   1
# Mrun200m 0.86 0.742 0.258   1
# Mrun800m 0.79 0.627 0.373   1
# 
# PC1
# SS loadings    4.46
# Proportion Var 0.64

# 6. What is the variance of the each of your extracted principal components?
# the h2 column shows variance

# 7. How much variability in the original dataset is captured by each of your extracted
# principal components
# 64% 

# 8. Create a plot of the principal component loadings against each other.
factor.plot(y_principal,labels=names(y))

# 9. Use the object you created in part 5 and the plot in part 8 to help construct
# interpretations for each principal component vector.
# This plot shows that there is something unique about the javelin that is not explained by PC1.
# We should porbably include the javalin and any other component to explain the result sufficiently.

# 10. Create a scatterplot of each of the competitor’s results projected onto the reduced
# dimensions.
plot(y_principal$scores)

# 11.Comment on any observations that appear to be outliers. Who are these competitors and why do they appear to be outliers?
# It appears that 25th indexed sample is an outlier.

# ------------------------------------------>
# Question #2: Ridge Regression
# ------------------------------------------>

# Read in the  [07] Prostate.txt dataset into your workspace. This dataset comes from a study by Stamey et a. (1989) of prostate cancer, measuring the correlation between the level of a prostate-specific antigen and some covariates. The included variables are the log-cancer volume, log-prostate weight, age of patient, log-amount of benign hyperplasia, seminal vesicle invasion, log-capsular penetration, Gleason score, and percent of Gleason scores 4 or 5; the response variable is the log-psa.
data <- read.table("~/Documents/NYCDSA/Course Work/CurseOfDimensionality/[07] The Curse of Dimensionality Homework/[07] Prostate.txt", quote="\"", comment.char="")
# 1. Create a training set of approximately 80% of your data and a test set of approximately 20% of your data ( NB: Use  set.seed(0) so your results will be reproducible.)
set.seed(0)
train = sample(1:nrow(data),0.7*nrow(data))
test  = (-train)
data.train<- data[train,]
data.test <-data[test,]

# 2. Fit a slew of ridge regression models  on your training data by checking across a wide range of lambda values. Save the coefficients of these models in an object.
library(glmnet)
grid = 10^seq(5, -2, length = 100)
y<-data.train$lpsa
x<-model.matrix(lpsa~.,data.train)
ridge.models = glmnet(x, y, alpha = 0, lambda = grid)

# 3. Plot the coefficients of these models and comment on the shrinkage.
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")
# After lambda 1, all the the coeffcients begin to shrink drastically and by 5 they are all ~ 0.
# The largest regression slope is about 0.8

# 4. Perform 10-fold cross-validation  on your training data and save the output as an object. Once again, use  set.seed(0)  . ( NB: You can manually define the values of lambda to as you did in part 2).
set.seed(0)
cv.ridge.out = cv.glmnet(x, y, lambda = grid, alpha = 0, nfolds = 10)    

# 5. Create and interpret a plot associated with the 10-fold cross-validation completed
# in part 4.
plot(cv.ridge.out, main = "Ridge Regression")
# The curve is sigmoidal and their is large MSE for large lambda. 

# 6. What is the best lambda?
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)
# -2.000225

# 7. What is the  test  MSE associated with this lambda value?
x.test<-model.matrix(lpsa~.,data.test)
y.test<-data.test$lpsa
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x.test)
mean((ridge.bestlambdatrain - y.test)^2)
# 0.4577852

# 8. Refit the ridge regression using the best lambda  using every observation in your
# original dataset . Briefly comment on the coefficient estimates.
x.total<-model.matrix(lpsa~.,data)
y.total<-data$lpsa
ridge.out<-glmnet(x.total,y.total,alpha=0)
predict(ridge.out, type = "coefficients", s = bestlambda.ridge)
# 10 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept) -0.037875472
# (Intercept)  .          
# lcavol       0.460663176
# lweight      0.591205148
# age         -0.014698468
# lbph         0.081017533
# svi          0.653722586
# lcp         -0.014635729
# gleason      0.068064248
# pgg45        0.003106984

#lcavol, lweight, and svi are all comparable and the most important given their larger magnitudes. 

# 9. What is the overall MSE for the model you fit in part 8? How does this compare to
# the MSE you found in part 7?
ridge.bestlambda<-predict(ridge.out, s = bestlambda.ridge, newx = x.total)
mean((ridge.bestlambda-y.total)^2)
# 0.4549841

# The full model just barely beats out the training model, which is not entirely surpising. The full model, 
# has more data points in this case that fit within the trend of the model and thus the MSE is slightly lower. 

# ------------------------------------------>
# Question #3: Lasso Regression
# ------------------------------------------>
# Continue using the  [07] Prostate.txt dataset you already loaded into your workspace.
# 1. Repeat the entire analysis performed in question #2, except use the method of lasso regression instead.
grid = 10^seq(5, -2, length = 100)
y<-data.train$lpsa
x<-model.matrix(lpsa~.,data.train)
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

set.seed(0)
cv.lasso.out = cv.glmnet(x, y, lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression")

bestlambda.lasso = cv.lasso.out$lambda.min
log(bestlambda.lasso)
#-3.465507

x.test<-model.matrix(lpsa~.,data.test)
y.test<-data.test$lpsa
lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x.test)
mean((lasso.bestlambdatrain - y.test)^2)
#0.4473154

lasso.out = glmnet(x, y, alpha = 1)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)

# (Intercept)  0.608868302
# (Intercept)  .          
# lcavol       0.515741003
# lweight      0.533559441
# age         -0.022553311
# lbph         0.107653028
# svi          0.440862807
# lcp          .          
# gleason      0.058509709
# pgg45        0.003798069

lasso.bestlambda = predict(lasso.out, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)
# 0.4755363

# 2. Compare and contrast your ultimate ridge and lasso models. Which would you choose to use? Why?
# The ridge regression has a lower MSE ( 0.4549841 vs 0.4755363), but the lasso regressions coefficients are smaller.
