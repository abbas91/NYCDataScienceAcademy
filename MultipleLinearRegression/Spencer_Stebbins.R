# ---------------------------------------------------------------------->
# Question #1: Multiple Linear Regression on New York City Restaurants
# ---------------------------------------------------------------------->
data <- read.csv("~/Documents/NYCDSA/Course Work/MultipleLinearRegression/MultipleLinearRegressionHomework/[04] NYC Restaurants.txt", sep="")
data <- as.data.frame(data)
# 1. Create a scatterplot matrix of all continuous variables colored by Location. From this plot alone, do you see any problems that might arise for multiple linear regression?
plot(data[-6], col=data$Location, main = "Scatterplot of NYC Restaurants Dataset")

# 2. Fit a multiple linear regression predicting the price of a meal based on the customer views and location of the restaurant. For this model:
    # a. Write out the regression equation.
    model = lm(Price ~ .  - Restaurant,data = data)
    
    # b. Interpret the meaning each of the 5 coefficients in context of the problem.
    summary(model) 
    # Residuals:
    #     Min       1Q   Median       3Q      Max 
    # -14.0465  -3.8837   0.0373   3.3942  17.7491 
    # 
    # Coefficients: (1 not defined because of singularities)
    # Estimate Std. Error t value Pr(>|t|)    
    # (Intercept)      -24.023800   4.708359  -5.102 9.24e-07 ***
    #     Food           1.538120   0.368951   4.169 4.96e-05 ***
    #     Decor          1.910087   0.217005   8.802 1.87e-15 ***
    #     Service       -0.002727   0.396232  -0.007   0.9945    
    #     LocationEast   2.068050   0.946739   2.184   0.0304 *  
    #     LocationWest         NA         NA      NA       NA    
    # ---
    #     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    # 
    # Residual standard error: 5.738 on 163 degrees of freedom
    # Multiple R-squared:  0.6279,	Adjusted R-squared:  0.6187 
    # F-statistic: 68.76 on 4 and 163 DF,  p-value: < 2.2e-16
    #   
    
    # c. Are the coefficients significant? How can you tell?
    # Based on teh p values, it seems all the coefficients are significant except for Service
    
    # d. Is the overall regression significant? How can you tell?
    # the overall regression is signifigant because the p value of the the f-test is far below .05
    
    # e. Find and interpret the RSE.
    # 5.738, this is an average residuals tend to deviate around the regression line.
    
    # f. Find and interpret the adjusted coefficient of determination.
    # .6279, which means that approximately 63% of the variability
    # in the Price  variable is explained by the other variables

# 3. Investigate the assumptions of the model using the plot()function. Are there any violations?
plot(model)
# Lineariy: There appears to be a linear relationship because the residuals vs fitted line is flat
# Normality: The data appears to be normally distributed around the model because the points fit snuggly on the line except for maybe at the tail ends
# Constant Variance: There appears to be relatively constant variance because the fitted line is flat
# Independent Errors: The data points appear to have constant variance and there is no pattern to the errors themselves,
# implying the errors are independent

# 4. Investigate the influence plot for the model. Are there any restaurants about which we should be concerned?
influencePlot(model)
# It appears that there are no restaurants that are in the top or bottom right boxes, implying high leverage and residual
# However, it does sema that resturant 130 is a point to be aware of
data[130,]$Restaurant #Rainbow Grill

# 5. Investigate the coefficient variance inflation factors; use these values to discuss multicollinearity.
library(car)
vif(model)
# Error in vif.default(model.reduced) : 
#     there are aliased coefficients in the model

# 6. Create added variable plots for this model. What conclusions might you draw from these plots?
avPlots(model) 
#based on these plots, it appears that Food and Decor are the greatest contirbutors to the model

# 7. Fit a new simple linear regression that predicts the price of dinner from the service rating alone. Discuss this regression in light of your answer to part 6.
model2 = lm(Price ~ Service, data = data)
summary(model2) 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -17.6646  -4.7540  -0.2093   4.3368  26.2460 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -11.9778     5.1093  -2.344   0.0202 *  
#     Service       2.8184     0.2618  10.764   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.153 on 166 degrees of freedom
# Multiple R-squared:  0.4111,	Adjusted R-squared:  0.4075 
# F-statistic: 115.9 on 1 and 166 DF,  p-value: < 2.2e-16
# With just one variable, the model can fit a very good regression line

plot(model2)
influencePlot(model2)  #there appears to be more outlying points in this model than the previous, and 117
# being both one that is has a high residual value and leverage value
data[117,]$Restaurant # Veronica

# ---------------------------------------------------------------------->
# Question #2: Model Selection on New York City Restaurants
# ---------------------------------------------------------------------->

# 1. Regress the price of dinner onto the average customer food rating, decor rating, and the restaurant location. In context of this new model, comment on:
model.reduced = lm(Price ~ . - Service - Restaurant, data = data)
    # a. The model summary() output.
    # summary(model.reduced)
    # Residuals:
    #     Min       1Q   Median       3Q      Max 
    # -14.0451  -3.8809   0.0389   3.3918  17.7557 
    # 
    # Coefficients: (1 not defined because of singularities)
    # Estimate Std. Error t value Pr(>|t|)    
    # (Intercept)  -24.0269     4.6727  -5.142 7.67e-07 ***
    #     Food           1.5363     0.2632   5.838 2.76e-08 ***
    #     Decor          1.9094     0.1900  10.049  < 2e-16 ***
    #     LocationEast   2.0670     0.9318   2.218   0.0279 *  
    #     LocationWest       NA         NA      NA       NA    
    # ---
    #     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    # 
    # Residual standard error: 5.72 on 164 degrees of freedom
    # Multiple R-squared:  0.6279,	Adjusted R-squared:  0.6211 
    # F-statistic: 92.24 on 3 and 164 DF,  p-value: < 2.2e-16
    # all values seem to be signifigant, with LocationEast having being least signifigant

    # b. The assumptions of multiple linear regression.
    plot(model.reduced)
    # Lineariy: There appears to be a linear relationship because the residuals vs fitted line is very flat
    # Normality: The data appears to be normally distributed around the model because the points fit snuggly on the line except for maybe at the upper tail end
    # Constant Variance: There appears to be relatively constant variance because the fitted line is flat
    # Independent Errors: The data points appear to have constant variance and there is no pattern to the errors themselves,
    # implying the errors are independent  

    # c. The influence plot of the model.
    influencePlot(model.reduced)
    # this model seems to reduce the highly influential points
    
    # d. The variance inflation factors of the coefficients.
    vif(model.reduced)
    # Error in vif.default(model.reduced) : 
    #     there are aliased coefficients in the model
    
    # e. The added variable plots for the model.
    avPlots(model.reduced) 
    # as stated in Q2.1.a, it sems that Food and Decor contribute most to this model, whereas location less so.
    
# 2. Run a partial F-test to compare this model with the overall model you created in question 1. Interpret your results.
anova(model, model.reduced)
# Analysis of Variance Table
# 
# Model 1: Price ~ (Restaurant + Food + Decor + Service + Location) - Restaurant
# Model 2: Price ~ (Restaurant + Food + Decor + Service + Location) - Service - 
#     Restaurant
# Res.Df    RSS Df Sum of Sq  F Pr(>F)
# 1    163 5366.5                       
# 2    164 5366.5 -1  -0.00156  0 0.9945
# Because the P value for the F test is quite large, indicating that we retain the null hypothesis and 
# conclude that the Service variable is not informative in our model; we move forward with the reduced model.
    
# 3. Fit a new reduced model that predicts the price of dinner by only the average customer food rating and average customer decor rating. Briefly comment on the model assumptions.
model.reduced.further = lm(Price ~ . - Service - Restaurant -Location , data = data)
summary(model.reduced.further)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -14.945  -3.766  -0.153   3.701  18.757 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -24.5002     4.7230  -5.187 6.19e-07 ***
#     Food          1.6461     0.2615   6.294 2.68e-09 ***
#     Decor         1.8820     0.1919   9.810  < 2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.788 on 165 degrees of freedom
# Multiple R-squared:  0.6167,	Adjusted R-squared:  0.6121 
# F-statistic: 132.7 on 2 and 165 DF,  p-value: < 2.2e-16
# Both coefficients are highly signifigant in this model and we have also reduced the Adj R Squared. 

# 4. Compare each of the following models based on AIC:
library(MASS)
AIC(model, model.reduced, model.reduced.further) 
# df      AIC
# model                  6 1070.711
# model.reduced          5 1068.711
# model.reduced.further  4 1071.677

    # a. The overall model fitted in question 1.
    # The model has 6 variables and is second best fit compared to the other models

    # b. The overall model without the service variable fitted in question 2 part 1.
    # The model has 5 variables and is the best fit compared to the other models

    # c. The reduced model fitted in question 2 part 3.
    # The model has 4 variables and is worst fit compared to the other models

# 5. Compare each of the models based on BIC.
BIC(model, model.reduced, model.reduced.further)
# df      BIC
# model                  6 1089.454
# model.reduced          5 1084.330
# model.reduced.further  4 1084.173
# the most reduced model that only uses Food and Decor Rating is best only slightly 
    
# 6. Do you expect to see the results from part 4 and part 5? Which model would you ultimately choose to use?
# The results of 4 and 5, implies that model.reduced is the best model based on the AIC, and the model.reduced.further
# is just barely the best based on the BIC. 
