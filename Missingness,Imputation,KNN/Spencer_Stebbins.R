#--------------------------------------------------------
#Question #1: Missingness & Imputation for the Titanic Dataset
#--------------------------------------------------------

library(PASWR)
data(titanic3)
data <- titanic3

#1
#a
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)
# columns with missing values
# age            263
# fare             1
# body          1188

#b
na_percent <-sapply(data, function(y) sum(length(which(is.na(y)))) / length(y) )
data.frame(na_percent)
# columns with missing values
# age       0.2009167303
# fare      0.0007639419
# body      0.9075630252

#2
sum(complete.cases(data)) / nrow(data)
# 0.09090909

#3
sum(data.frame(na_count)) / (nrow(data) * ncol(data))
#0.07923169

#4
library(VIM)
library(mice)
aggr(data)
md.pattern(data)
# pclass survived name sex sibsp parch ticket cabin embarked boat home.dest fare age body     
# 119      1        1    1   1     1     1      1     1        1    1         1    1   1    1    0
# 1      1        1    1   1     1     1      1     1        1    1         1    1   0    1    1
# 1      1        1    1   1     1     1      1     1        1    1         1    0   1    1    1
# 926      1        1    1   1     1     1      1     1        1    1         1    1   1    0    1
# 262      1        1    1   1     1     1      1     1        1    1         1    1   0    0    2
# 0        0    0   0     0     0      0     0        0    0         0    1 263 1188 1452

#5
#I can't assume to know why the missing data is missing thus I cannot determine whether the missing data is MCAR, MNAR, or MAR. 

#6
plot(density(data$age, na.rm=TRUE))
data$age[is.na(data$age)] = mean(data$age, na.rm=TRUE)
plot(density(data$age))
# using mean imputation causes the density distrubtion to become much narrower around the mean and decreases 
# standard deviation as we would expect

#7
data <- titanic3
plot(density(data$age, na.rm=TRUE))
data$age <- sapply(data$age, function(y) ifelse(is.na(y),sample(na.omit(data$age), 1), y))
plot(density(data$age))
# using simple random imputation causes the density distrubtion to alter only slightly. The problem I see with this method
# is that if there were substantial outliers that are picked at random a lot, the density plot would be skewed by these outliers
# that have been injected into the data set thus inducing bias

#--------------------------------------------------------
#Question #2: K-Nearest Neighbors with the Titanic Dataset
#--------------------------------------------------------

#1
data <- titanic3
x <- sample(na.omit(data$fare), 1)
#23.450008
data$fare <- sapply(data$fare, function(y) ifelse(is.na(y),x, y))

#2    
data$age <- sapply(data$age, function(y) ifelse(is.na(y),sample(na.omit(data$age), 1), y))
plot(data$fare,data$age,col=data$pclass)
#most people regardless of age skewed towards left towards lower fares and lower class. 

#3
data <- rbind(data,data.frame(pclass=NA,survived=NA,name=NA,sex="male",age=50,sibsp=NA,parch=NA,ticket=NA,fare=400.0,cabin=NA,embarked=NA,boat=NA,body=NA,home.dest=NA))
data <- rbind(data,data.frame(pclass=NA,survived=NA,name=NA,sex=NA,age=10,sibsp=NA,parch=NA,ticket=NA,fare=100.0,cabin=NA,embarked=NA,boat=NA,body=NA,home.dest=NA))
#4
#my guess is that they would both be in 1st class based on the plot from question 2

#5
imputed.1nn <- kNN(data, k = 1)
tail(imputed.1nn$pclass)
#       pclass
# 1310    1st       
# 1311    3rd  

#6
sqrt(nrow(data)) 
# 36.20773
imputed.36nn <- kNN(data,k=36)
tail(imputed.36nn)
#       pclass
# 1310    1st      
# 1311    2nd 

#--------------------------------------------------------
#Question #3: Minkowski Distances with the Titanic Dataset
#--------------------------------------------------------

#1
#a
data <- titanic3[c('pclass', 'survived', 'sex', 'age', 'sibsp', 'parch')]
#b
library(mice)
data$fare <- impute(data$fare, "random")
#2
complete_data <- data[complete.cases(data),]
missing_data <- data[!complete.cases(data$age),]

#3
library(kknn)
manhattan <- kknn(age ~ ., complete_data, missing_data, k=1, distance=1)
euclidien <- kknn(age ~ ., complete_data, missing_data, k=1, distance=2)
minkowski <- kknn(age ~ ., complete_data, missing_data, k=1, distance=10)

#4
manhattan <- as.data.frame(manhattan["fitted.values"])
euclidien <- as.data.frame(euclidien["fitted.values"])
minkowski <- as.data.frame(minkowski["fitted.values"])

manhattan$type<-'manhattan'
euclidien$type<-'euclidean'
minkowski$type<-'minkowski'
fitted <- rbind(manhattan, euclidien, minkowski)

org <- as.data.frame(data[,4])
names(org) <-'fitted.values'
org$type <-'original'

data <- rbind(org, fitted)
data$type <- factor(data$type)

library(ggplot2)
ggplot(data,aes(fitted.values,color=type))+geom_density()

# The original data has a smooth age distribution whereas all the rest have spikes with a large one around 25.

#5
k <- floor(sqrt(nrow(data)))
#36
manhattan<-kknn(age~., complete_data, missing_data, distance=2, k=k)
euclidean<-kknn(age~., complete_data, missing_data, distance=2, k=k)
minkowski<-kknn(age~., complete_data, missing_data, distance=10, k=k)

#6
manhattan <- as.data.frame(manhattan["fitted.values"])
euclidien <- as.data.frame(euclidien["fitted.values"])
minkowski <- as.data.frame(minkowski["fitted.values"])

manhattan$type<-'manhattan'
euclidien$type<-'euclidean'
minkowski$type<-'minkowski'
fitted <- rbind(manhattan, euclidien, minkowski)

data <- titanic3[c('pclass', 'survived', 'sex', 'age', 'sibsp', 'parch')]
org <- as.data.frame(data[,4])
names(org) <-'fitted.values'
org$type <-'original'

data <- rbind(org, fitted)
data$type <- factor(data$type)

library(ggplot2)
ggplot(data,aes(fitted.values,color=type))+geom_density()

# With the N squared knn, the non-normal distributions are converging around at age 25 
#  and look more alike in distribution compared to when k is 1 
