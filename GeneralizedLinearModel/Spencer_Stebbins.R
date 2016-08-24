#----------------------------------------->
# Question #1: Birdkeeping & Lung Cancer
#----------------------------------------->

# 1.Perform some basic numerical and graphical EDA. In particular, comment on the scatterplots of the continuous variables colored by whether or not an individual had lung cancer. What might be good? What might be bad?
library(Sleuth2)
data("case2002")
# LC   FM   SS     BK AG YR CD
# 1 LungCancer Male  Low   Bird 37 19 12
# 2 LungCancer Male  Low   Bird 41 22 15
# 3 LungCancer Male High NoBird 43 19 15
# 4 LungCancer Male  Low   Bird 46 24 15
# 5 LungCancer Male  Low   Bird 49 31 20
# 6 LungCancer Male High NoBird 51 24 15
summary(case2002) #Looking at the five number summary information.
# LC          FM         SS           BK           AG              YR              CD       
# NoCancer  :98   Male  :111   Low :102   NoBird:80   Min.   :37.00   Min.   : 0.00   Min.   : 0.00  
# LungCancer:49   Female: 36   High: 45   Bird  :67   1st Qu.:52.00   1st Qu.:20.00   1st Qu.:10.00  
# Median :59.00   Median :30.00   Median :15.00  
# Mean   :56.97   Mean   :27.85   Mean   :15.75  
# 3rd Qu.:63.00   3rd Qu.:39.00   3rd Qu.:20.00  
# Max.   :67.00   Max.   :50.00   Max.   :45.00
sapply(case2002, sd) #Looking at the individual standard deviations.
# LC         FM         SS         BK         AG         YR         CD 
# 0.4730162  0.4314969  0.4624569  0.4997437  7.3488560 13.9756901  9.7037230 
sapply(case2002, class) #Looking at the variable classes.
# LC        FM        SS        BK        AG        YR        CD 
# "factor"  "factor"  "factor"  "factor" "numeric" "numeric" "numeric" 
plot(case2002[-1], col = case2002$LC) #Basic graphical EDA.

# 2.Fit a logistic regression predicting whether or not an individual has lung cancer that includes all variables in the model.
model.all <- glm(LC ~., data=case2002, family="binomial")

# 3.Briefly assess the appropriate residual plot and an influence plot for the model created in part 2.
# created in part 2.
plot(model.all)
influencePlot(model.all)
# There is non-linearity on the residuals vs fitted values. Normal QQ plot shows significant
# tail and head deviation. Need to investigate #47, #28 

# 4.Conduct and interpret an overall goodness of fit test for the model created in part 2.
model.all$deviance
summary(model.all)
pchisq(model.all$deviance, model.all$df.residual, lower.tail = FALSE)
#Based on residual deviance < null deviance and the p-value of 0.1946336 is greater than .05, we accept the null hypothesis

# 5.Interpret the coefficient of gender on the log odds scale.
# Based on coefficients, females  have a larger (0.56127) probability of getting lung cancer.

# 6.Interpret the coefficient of socioeconomic status on the odds scale.
exp(model.all$coefficients)[3]
# Based on coefficients, people hav high social economic status have increased chance of getting cancer: 1.111208 probability

# 7.Interpret the 95% confidence interval based on standard errors for the birdkeeping indicator on the log odds scale.
confint.default(model.all)
# 95% confidence interval between 0.5565 and 2.1687 does not include 0 and thus we reject the null hypothesis

# 8.Interpret the 95% confidence interval based on standard errors for the years of smoking variable on the odds scale.
exp(confint.default(model.all))
# 95% confidence interval on standard errors on odds scale is bounded between 1.0212 and
# 1.1329 and does not include zero. Thus we reject the null hypothesis.

# 9.Fit a logistic regression predicting whether or not an individual has lung cancer that includes all variables in the model except the birdkeeping indicator.
model.no.BK <- glm(LC ~.-BK, data=case2002, family="binomial")

# 10.Conduct and interpret an overall goodness of fit test for the model created in part
model.no.BK$deviance
summary(model.no.BK)
pchisq(model.no.BK$deviance, model.no.BK$df.residual, lower.tail = FALSE)
# Based on the fact that residual deviance < null deviance and the p-value at 0.0748 is greater 
# than 0.05, we accept the null hypothesis.

# 11. Conduct and interpret a drop in deviance test comparing the two models you’ve created thus far. Which would you keep in favor of the other?
model.no.BK.deviance = model.no.BK$deviance    # Comparing the deviance of that with no BK
model.no.BK.df = model.no.BK$df.residual       # model (the one with no BK)
model.all.deviance = model.all$deviance        # the deviance of the full model
model.all.df = model.all$df.residual            
pchisq(model.no.BK.deviance - model.all.deviance, model.no.BK.df - model.all.df, lower.tail = FALSE)
# or alternatively, simpler to use...
anova(model.no.BK, model.all, test = "Chisq")
# Analysis of Deviance Table
# Model 1: LC ~ (FM + SS + BK + AG + YR + CD) - BK
# Model 2: LC ~ FM + SS + BK + AG + YR + CD
# Resid.   Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1       141     165.87                          
# 2       140     154.20  1    11.67 0.0006352 ***
# Model.all has low p value and thus is the better model

# 12. Fit a logistic regression predicting whether or not an individual has lung cancer based only on whether or not they have birds and the number of years they have been smoking.
model.BK.YR <- glm(LC ~ BK+YR, data=case2002, family="binomial")

# 13. Conduct and interpret a drop in deviance test comparing the model you created in part 12 to the model you created in part 2. Which would you keep in favor of the other?
anova(model.BK.YR, model.all, test = "Chisq")
# Analysis of Deviance Table
# Model 1: LC ~ BK + YR
# Model 2: LC ~ FM + SS + BK + AG + YR + CD
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1     144     158.11                     
# 2     140     154.20  4    3.916   0.4175
# P value of model 2 is not signifigant and thus model 1 is better logistical regression model

# 14. Compare the models across:
    # a. AIC
    #AIC(model.BK.YR, model.all)
    #             df      AIC
    # model.BK.YR  3 164.1144
    # model.all    7 168.1984
    # Answer: Model with just the BK and YR is better, since it has the lower AIC.
    
    # b. BIC
    BIC(model.BK.YR, model.all)
    #             df      BIC
    # model.BK.YR  3 173.0857
    # model.all    7 189.1314
    # Answer: Model with just the BK and YR is better, since it has the lower BIC.
    
    # c. R^2 deviance
    1-model.BK.YR$deviance/model.BK.YR$null.deviance
    # Answer: Resulting R^2 deviance comes in at 0.1550791. 
    
    # d. Give an argument for choosing the model created in part 12.
    # 15.5% of the variability appears to be explained by the predictors in Model.BK.YR, which is not very much even though
    # it is the better model
    
# 15. Using the model created in part 12, predict:
    # a. The probability of having lung cancer for an individual with an average
    # number of years smoking with and without birds within their household.
    newdata1 = with(case2002, data.frame(BK="Bird",YR=mean(YR)))
    newdata1
    predict(model.BK.YR, newdata1, type="response")*100
    # 47.94% probability of having lung cancer with bird ownership and avg years of smoking
    newdata2 = with(case2002, data.frame(BK="NoBird",YR=mean(YR)))
    newdata2
    predict(model.BK.YR, newdata2, type="response")*100
    # 17.395% probability of having lung cancer with no bird ownership and avg years of smoking
    
    # b. The probability of having lung cancer for an individual with no years prior
    # smoking with and without birds within their household.
    newdata3 = with(case2002, data.frame(BK="Bird",YR=0))
    newdata3
    predict(model.BK.YR, newdata3, type="response")*100
    # 15.39% probability of having lung cancer with bird ownership and no years smoking
    newdata4 = with(case2002, data.frame(BK="NoBird",YR=0))
    newdata4
    predict(model.BK.YR, newdata4, type="response")*100
    # 3.99% probability of having lung cancer with no bird ownership and no years smoking
    
# 16. Use the model created in part 12 to classify the observations in your dataset as having or not having lung cancer. Comment on how well the model performs as
# compared to the baseline.
predicted.LC <- round(model.BK.YR$fitted.values)
table(Truth=case2002$LC, Prediction=predicted.LC)
#          Prediction
# Truth       0  1
# NoCancer   85 13
# LungCancer 27 22
(85+22)/nrow(case2002)*100    # = 72.79% as the model prediction
(85+13)/nrow(case2002)*100    # = 66.67% as the no cancer benchmark
(27+22)/nrow(case2002)*100    # = 33.33% as the cancer benchmark

# Logistic regression model prediction is preffered.