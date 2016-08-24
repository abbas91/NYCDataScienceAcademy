#-----------------------------------------------
#Question #1: Anatomical Data from Domestic Cats#
#-----------------------------------------------
library(MASS)
data(cats)
cats <- cats


# 1. Create a scatterplot of heart weight versus body weight. From this plot alone, 
#do you think simple linear regression would be a good fit for the data? Why?
plot(x=cats$Hwt, y=cats$Bwt, main= "Scatterplot of Cats Dataset")
# Based on the scatterplot created above, it seems as if a simple linear regression
# would be a good fit for the data because there appears to be linearity,
# constance variance and normaliy

# 2. Regress heart weight onto body weight. For this model:

    # a. Write out the regression equation.
    model = lm(Bwt ~ Hwt, data = cats) 
    summary(model)
    
    # b. Interpret the meaning of the coefficients in context of the problem.
    summary(model)$coefficients
    # Call:
    #     lm(formula = Bwt ~ Hwt, data = cats)
    # 
    # Residuals:
    #     Min       1Q   Median       3Q      Max 
    # -0.58283 -0.22140 -0.00879  0.20825  0.91717 
    # 
    # Coefficients:
    #     Estimate Std. Error t value Pr(>|t|)    
    # (Intercept) 1.019637   0.108428   9.404   <2e-16 ***
    #     Hwt         0.160290   0.009944  16.119   <2e-16 ***
    #     ---
    #     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    # 
    # Residual standard error: 0.2895 on 142 degrees of freedom
    # Multiple R-squared:  0.6466,	Adjusted R-squared:  0.6441 
    # F-statistic: 259.8 on 1 and 142 DF,  p-value: < 2.2e-16
    # Based on coefficients, when Hwt = 0, Bwt = 1.02 and for every 1 more of Hwt, Bwt increases by .16 
    
    # c. Are the coefficients significant? How can you tell?
    # Yes because both p value for the intercept and hwt are belwo .05
    
    # d. Is the overall regression significant? How can you tell? How does the answer to part c. relate?
    # Yes because p value of the F-Test is far below a p value of .05
    
    # e. Find and interpret the RSE.
    # .2895
    # This means that on average the model has an error of only .2895 for an given point in the data
    
    # f. Find and interpret the coefficient of determination.
    summary(model)$r.squared 
    # 0.6466209
    # The value of .64 means that this model is a pretty good fit for the data as 65% of Bwt is expained by Hwt
    
# 3. Add the regression line to your plot from part 1.
beta1 = sum((cats$Hwt - mean(cats$Hwt)) * (cats$Bwt- mean(cats$Bwt))) / sum((cats$Hwt - mean(cats$Hwt))^2)
beta0 = mean(cats$Bwt) - beta1*mean(cats$Hwt)   
abline(beta0, beta1, lty = 2)

# 4. Add a visualization of the residuals to your plot from part 3. Do any of the
# residuals seem abnormally large?
segments(cats$Hwt, cats$Bwt, cats$Hwt, (beta0 + beta1*cats$Hwt), col = "red")

# 5. Construct 95% confidence intervals for the model coefficients. Interpret the
# intervals in context of the problem.
confint(model)
# 2.5 %    97.5 %
#     (Intercept) 0.8052956 1.2339778
# Hwt         0.1406330 0.1799475
# These interval means that for every increase of 1 unit in Hwt, Bwt will increase between .14
# and .18 with 95% confidence 

# 6. Assess each of the assumptions of the model.
plot(model)
# Lineariy: There appears to be a linear relationship because the residuals vs fitted line is flat
# Normality: The data appears to be normally distributed around the model because the points fit snuggly on the line
# Constant Variance: There appears to be relatively constant variance because the fitted line is flat
# Independent Errors: The data points appear to have constant variance and there is no pattern to the errors themselves,
# implying the errors are independent

# 7. Redraw the scatterplot and regression line from part 3 and add both confidence and prediction bands.
plot(x=cats$Hwt, y=cats$Bwt, main= "Scatterplot of Cats Dataset")
abline(model, lty = 2) #Plotting the regression line.
newdata = data.frame(Hwt=cats$Hwt)
conf.band = predict(model, newdata, interval = "confidence")
pred.band = predict(model, newdata, interval = "prediction")
lines(newdata$Hwt, conf.band[, 2], col = "blue") #Plotting the lower confidence band.
lines(newdata$Hwt, conf.band[, 3], col = "blue") #Plotting the upper confidence band.
lines(newdata$Hwt, pred.band[, 2], col = "red") #Plotting the lower prediction band.
lines(newdata$Hwt, pred.band[, 3], col = "red") #Plotting the upper prediction band.
    # a. Why is the prediction band wider than the confidence band?
    # The prediction band is wider than the confidence interval because it is estimating a single point as opposed
    # the whole data set

    # b. Why does the confidence band widen as it travels away from the center of
    # the regression line?
    # Because it is more likely that average y values for a given x will be tigther as you aproach the mean.

# 8. Construct confidence and prediction intervals for body weights of 2.8 kg, 5 kg, and
# 10 kg. Do you foresee any issues with reporting any of these intervals?
newdata = data.frame(Hwt = c(2.8, 5, 10)) 
predict(model, newdata, interval = "confidence") 
# fit      lwr      upr
# 1 1.468449 1.307303 1.629596
# 2 1.821088 1.700569 1.941607
# 3 2.622539 2.573263 2.671815
predict(model, newdata, interval = "prediction")
# fit       lwr      upr
# 1 1.468449 0.8738921 2.063007
# 2 1.821088 1.2362332 2.405943
# 3 2.622539 2.0481192 3.196959
# Both 2.8 and 5 exists outside the range of the original dataset, so we are extropolating thier 
# values based on the regression model.

#-----------------------------------------------
#Question #2: Considering Transformations
#-----------------------------------------------

# 1. Create a Box-Cox plot for transforming the  catsregression you created in question 1 above.
bc = boxcox(model) 

# 2. Determine the best value of lambda for a Box-Cox transformation on the  cats regression. (Hint: Try to balance interpretability and accuracy; when taking this perspective, there isn’t really a completely “correct” answer.)
lambda = bc$x[which(bc$y == max(bc$y))]
# 0.262626

# 3. Transform your data based on your answer to part 2.
dist.bc = (cats$Bwt^lambda - 1)/lambda #Applying the Box-Cox transformation.

# 4. Construct a new regression now using the transformed data.
model.bc = lm(dist.bc ~ cats$Hwt, data=cats)

# 5. Create a scatterplot of the transformed data and overlay the new regression line.
plot(x=cats$Hwt, y=dist.bc, main= "Scatterplot of Cats Dataset")
beta1 = sum((cats$Hwt - mean(cats$Hwt)) * (dist.bc- mean(dist.bc))) / sum((cats$Hwt - mean(cats$Hwt))^2)
beta0 = mean(dist.bc) - beta1*mean(cats$Hwt)   
abline(beta0, beta1, lty = 2)

# 6. Inspect the summary information and validate the assumptions of the linear
# regression model. Is there anything to be concerned about in the new model?
plot(model.bc) 
# Lineariy: There appears to be a linear relationship because the residuals vs fitted line is flat except maybe on the high end
# Normality: The data appears to still be normally distributed around the model because the points fit snuggly on the line
# Constant Variance: There appears to be relatively constant variance because the fitted relatively line is flat
# Independent Errors: The data points appear to still have constant variance and there is no pattern to the errors themselves,
# implying the errors are independent

# 7. Compare the models you created:
summary(model)
# Call:
#     lm(formula = Bwt ~ Hwt, data = cats)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.58283 -0.22140 -0.00879  0.20825  0.91717 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.019637   0.108428   9.404   <2e-16 ***
#     Hwt         0.160290   0.009944  16.119   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2895 on 142 degrees of freedom
# Multiple R-squared:  0.6466,	Adjusted R-squared:  0.6441 
# F-statistic: 259.8 on 1 and 142 DF,  p-value: < 2.2e-16

summary(model.bc)
# lm(formula = dist.bc ~ cats$Hwt)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.28661 -0.11047  0.00392  0.10001  0.40223 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.336634   0.051838   6.494 1.31e-09 ***
#     cats$Hwt    0.074760   0.004754  15.725  < 2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1384 on 142 degrees of freedom
# Multiple R-squared:  0.6352,	Adjusted R-squared:  0.6327 
# F-statistic: 247.3 on 1 and 142 DF,  p-value: < 2.2e-16

# a. Give one reason why you might use the original model instead of the Box-Cox transformed model.
# Because we lose the original interpretation of the data as we are comparing power of Bwt when using the box cox model

# b. Give one reason why you might use the Box-Cox transformed model instead of the original model.
# Because it has a smaller coefficient of determination, implying it will work better fit to the data

# 8. Attempt to apply a Box-Cox transformation on the model on which you already applied a Box-Cox transformation. What happens?
boxcox(model.bc) 
#when performing a boxcox transformation on the box cox model, you can see that the interval straddles 1,
#which implies that a further transofrmation has no effect
