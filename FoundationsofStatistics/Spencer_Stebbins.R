library(ggplot2)
library(datasets)

##Q1)
#1
data <- read.csv("~/Documents/NYCDSA/Course Work/FoundationsofStatistics/[01] Temp.txt", sep="")
summary(data)
# Body.Temp         Gender     Heart.Rate   
# Min.   : 96.30   Female:65   Min.   :57.00  
# 1st Qu.: 97.80   Male  :65   1st Qu.:69.00  
# Median : 98.30               Median :74.00  
# Mean   : 98.25               Mean   :73.76  
# 3rd Qu.: 98.70               3rd Qu.:79.00  
# Max.   :100.80               Max.   :89.00 

#2
ggplot(data)+ geom_boxplot(aes(Gender,Body.Temp))
ggplot(data)+ geom_boxplot(aes(Gender,Heart.Rate))
#3
#a)
t.test(data$Body.Temp, mu = 98.6)
# data:  data$Body.Temp
# t = -5.4548, df = 129, p-value = 2.411e-07
# alternative hypothesis: true mean is not equal to 98.6
# 95 percent confidence interval:
#     98.12200 98.37646
# sample estimates:
#     mean of x 
# 98.24923 
    
#b)
# 98.6 does not fall within the 95% confidence interval of the sample data ( 98.12200 98.37646) and the sample
# test has a p-value of 2.41e-7 which is far below .05 implying that we should reject the null hypothesis

#4
#a) 
var.test(data$Body.Temp[data$Gender == "Male"],
         data$Body.Temp[data$Gender == "Female"],
         alternative = "two.sided")
# data:  data$Body.Temp[data$Gender == "Male"] and data$Body.Temp[data$Gender == "Female"]
# F = 0.88329, num df = 64, denom df = 64, p-value = 0.6211
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#     0.5387604 1.4481404
# sample estimates:
#     ratio of variances 
# 0.8832897

#b)
#Because the p value of the F-Test is .6211 which is far greater than .05, we can conclude the 
#alternative hypothesis is not statisically significant.

#5
#a)
var.test(data$Heart.Rate[data$Gender == "Male"],
         data$Heart.Rate[data$Gender == "Female"],
         alternative = "greater")

#b)
#The F test has a p value of .9945 which is far greater than .05 meaning that we should reject the null hypothesis 
#female and males have equal heart rates and conclude that the hypothesis that males have a higher heart rate is 
#statistically signifigant

##Q2)
#1
data(PlantGrowth)
data <- PlantGrowth
ggplot(data)+ geom_boxplot(aes(group,weight))
summary(data[data$group == "ctrl",])
# weight       group   
# Min.   :4.170   ctrl:10  
# 1st Qu.:4.550   trt1: 0  
# Median :5.155   trt2: 0  
# Mean   :5.032            
# 3rd Qu.:5.293            
# Max.   :6.110
summary(data[data$group == "trt1",])
# weight       group   
# Min.   :3.590   ctrl: 0  
# 1st Qu.:4.207   trt1:10  
# Median :4.550   trt2: 0  
# Mean   :4.661            
# 3rd Qu.:4.870            
# Max.   :6.030 
summary(data[data$group == "trt2",])
# weight       group   
# Min.   :4.920   ctrl: 0  
# 1st Qu.:5.268   trt1: 0  
# Median :5.435   trt2:10  
# Mean   :5.526            
# 3rd Qu.:5.735            
# Max.   :6.310  

#2
bartlett.test(weight~group,data=PlantGrowth)
# data:  weight by group
# Bartlett's K-squared = 2.8786, df = 2, p-value = 0.2371

#They do not differ signifigantly because the p value of the bartlett test is above .05

#3
summary(aov(PlantGrowth$weight~PlantGrowth$group))

# Df Sum Sq Mean Sq F value Pr(>F)  
# PlantGrowth$group  2  3.766  1.8832   4.846 0.0159 *
#     Residuals         27 10.492  0.3886  

#Because the p value is so small, we reject the null hypothesis that all treaments have the same mean weight

#Q3)
#1
#a)
data(HairEyeColor)
data <- HairEyeColor
mosaicplot(HairEyeColor,shade=TRUE)

#b)
# Blonde females with blue eyes have higher count than expected
# Blonde hair males and females with brown eyes have fewer than expected count

#2
data <- as.data.frame(data)
mosaicplot(HairEyeColor[,1:2,Sex='Female'],shade=T)
# Blondes females with blue eyes are higher than expected and blond with brown eyes are less than expected

#3
chisq <- chisq.test(HairEyeColor[,1:2,Sex='Female'])
chisq$residuals
        #Eye
# Hair        Brown      Blue
# Black  2.640869 -2.731960
# Brown  1.989603 -2.058231
# Red    1.191988 -1.233103
# Blond -5.254303  5.435539
#Blonde females with blue high have the highest residual

