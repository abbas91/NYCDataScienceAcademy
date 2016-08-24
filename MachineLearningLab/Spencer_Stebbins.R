#Data Analysis

###################################################################
###################################################################
#####[05] Machine Learning Lab: Building Bridges Solution Code#####
###################################################################
###################################################################



#############################
#####Basic Data Analyses#####
#############################
bridges <- read.csv("~/Documents/NYCDSA/Course Work/MachineLearningLab/[05] Machine Learning Lab Building Bridges/[05] Bridges.txt", sep="")


#1
summary(bridges)
sapply(bridges, sd)
sd(bridges$Time[-(45:50)])

#2ab
dwgs.category = NULL
dwgs.category[which(bridges$Dwgs %in% 3:5)] = "3-5 Drawings"
dwgs.category[which(bridges$Dwgs %in% 6:8)] = "6-8 Drawings"
dwgs.category[which(bridges$Dwgs %in% 9:15)] = ">8 Drawings"
#Could also use:
#dwgs.category = cut(bridges$Dwgs,
#                    c(3, 5, 8, 15),
#                    include.lowest = TRUE)

span.category = NULL
span.category[which(bridges$Spans == 1)] = "1 Span"
span.category[which(bridges$Spans != 1)] = ">1 Span"
#Could also use:
#span.category = cut(bridges$Spans, c(0, 1, 7))


#3
table(dwgs.category, span.category)
chisq.test(dwgs.category, span.category)

#The variables are not independent of one another.

#4ab
library(VIM)
aggr(bridges)

sum(is.na(bridges))
sum(is.na(bridges))/nrow(bridges)
sum(is.na(bridges))/(nrow(bridges)*ncol(bridges))

#There are 5 missing values in the Time veriable, which accounts for 10% of the
#Time variable, and about 1.67% of the cells in our dataset overall.

#5
bridges.imputed = kNN(bridges, k = sqrt(nrow(bridges)))[, 1:6]

#Data Modeling-----------

#1
plot(imputed.data$Time, imputed.data$Dwgs)

#2
model <- lm(Time ~ Dwgs,data=imputed.data)
summary(model)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -103.06  -32.95  -10.40   18.42  200.37 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -24.511     23.160  -1.058    0.295    
# Dwgs          23.863      2.922   8.165 1.24e-10 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 60.71 on 48 degrees of freedom
# Multiple R-squared:  0.5814,	Adjusted R-squared:  0.5727 
# F-statistic: 66.67 on 1 and 48 DF,  p-value: 1.24e-10

# This model is very signifigant because of the low p value, but R squared is high implying that we can only explain
# 58% of the variance of our obversations

#3
library(MASS)
bc <- boxcox(model)
lambda = bc$x[which(bc$y == max(bc$y))]
Time.bc = (imputed.data$Time^lambda - 1)/lambda 
model.bc = lm(Time.bc ~ Dwgs, data=imputed.data)
summary(model.bc)
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.71685 -0.20724  0.00254  0.19158  0.94951 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.5509     0.1308   27.16  < 2e-16 ***
#     Dwgs          0.1437     0.0165    8.71  1.9e-11 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3427 on 48 degrees of freedom
# Multiple R-squared:  0.6125,	Adjusted R-squared:  0.6044 
# F-statistic: 75.87 on 1 and 48 DF,  p-value: 1.899e-11

# This increased the R squared value, but only slightly impling this model now fits 61% of the obvservatons
# The intercept is now signifigant as well

#4
# Time = 3.5509 + .1437(Dwgs) 
# For every 1 additional drawing, time increases by .1437 person days

#5
saturated.model <- lm(log(Time) ~ . -Time, data= imputed.data)
summary(saturated.model)
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.67881 -0.22496  0.00071  0.25944  0.66904 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.6951819  0.1346844  27.436  < 2e-16 ***
# DArea       -0.0046460  0.0070018  -0.664   0.5104    
# CCost        0.0002506  0.0002252   1.113   0.2718    
# Dwgs         0.1139285  0.0214780   5.304 3.51e-06 ***
# Length       0.0004058  0.0003309   1.226   0.2266    
# Spans        0.0839572  0.0442828   1.896   0.0645 .  
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3373 on 44 degrees of freedom
# Multiple R-squared:  0.7176,	Adjusted R-squared:  0.6855 
# F-statistic: 22.36 on 5 and 44 DF,  p-value: 4.358e-11

# The model is signifigant because the p value is low. 
# Dwgs is the only really signifigant variable and we can explain 68% of vairance given sdj r square
plot(saturated.model)
library(car)
influencePlot(saturated.model)
# Lineariy: This model may be a bit skewed because the line in the residual vs fitted plot is curved.
# Normality: The data seems to be relatively normal, but skews heavily at the upper end and has a dip in the middle
# Constant Variance: There appears to be constant variance because the data is consistently evenly distributed above and below the regression line
# Independent Errors: There may be independent errors because the data is not entirely normal.
# 33, 47 and 48 are influential points that could be skewing the data

#6
bc <- boxcox(saturated.model)
lambda = bc$x[which(bc$y == max(bc$y))]
Time.bc = (imputed.data$Time^lambda - 1)/lambda 
saturated.model.bc = lm(Time.bc ~ . -Time, data=imputed.data)
# This box cox plot shows that it includes 1 in the 95% confidence meaning we dont need to transofrm the data

#7
plot(imputed.data)
vif(saturated.model)
# DArea    CCost     Dwgs   Length    Spans 
# 2.432283 2.285397 1.749917 2.238713 2.242378 
avPlots(saturated.model)
# Because none of the variables have a vif above 5, this implies multicolinearity is minimal if exists.
# Based on the av plots, it appears that only Dwgs is a signifigant estimator of Time in this model.

#8
model.empty = lm(log(Time) ~ 1, data = imputed.data) #The model with an intercept ONLY.
scope = list(lower = formula(model.empty), upper = formula(saturated.model))
forwardAIC = step(saturated.model, scope, direction = "forward", k = 2)
backwardAIC = step(saturated.model, scope, direction = "backward", k = 2)
bothAIC = step(saturated.model, scope, direction = "both", k = 2)


