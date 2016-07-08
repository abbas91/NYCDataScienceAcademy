#Q1------------
TimesSquareSignage <- read.csv("~/Documents/NYCDSA/HW/Intro R/Archive/TimesSquareSignage.csv")
#1
nrow(TimesSquareSignage)
ncol(TimesSquareSignage)
#2
sapply(TimesSquareSignage, function(x) class(x))
#3
sum(is.na(TimesSquareSignage))
#4
rowSums(is.na(TimesSquareSignage))
colSums(is.na(TimesSquareSignage))

#Q2------------
#1
upperBway = subset(TimesSquareSignage, Location == 'Upper Bway')
write.csv(upperBway, file='upperBway_data.csv', sep=',', row.names = F)
#2
greaterSqft = subset(TimesSquareSignage, TOTAL.SF > mean(TimesSquareSignage$TOTAL.SF))
write.csv(greaterSqft, file='greaterSqft_data.csv', sep=',', row.names = F)
#3
head(data.frame(greaterSqft$Screen.Name..LED...Vinyl.Signs., greaterSqft$Building.Address, greaterSqft$Location))

#Q3------------
#1
data(cars)
#2
cars[0:5,]
#3
state = sample(c('NY', 'CT', 'CA'), size=nrow(cars), replace=TRUE)
#4
cars$state = state
#5
cars$ratio = cars$dist / cars$speed
mean(cars$ratio)
sd(cars$ratio)
