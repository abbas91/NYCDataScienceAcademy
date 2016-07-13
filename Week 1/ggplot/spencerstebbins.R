#Q1----------
library(ggplot2)
library(dplyr)
Champions <- read.csv("~/Documents/NYCDSA/Course Work/Week 1/ggplot/Data Visualization Homework/Champions.csv")
#1
tbl_df = filter(Champions, HomeGoal > AwayGoal)
filter(Champions, HomeTeam == "Barcelona" | HomeTeam ==  "Real Madrid")
#2
home_df = select(Champions, starts_with('Home'))
teams_df = select(Champions, HomeTeam, AwayTeam, HomeGoal, AwayGoal, HomeCorner, AwayCorner)
#3
arrange(teams_df, HomeGoal)
#4
Champions %>% group_by(HomeTeam) %>% summarise(avg_home_goals=mean(HomeGoal),avg_home_possession=mean(HomePossession), avg_home_yellow=mean(HomeYellow))
#5
Champions$score = Champions$HomeGoal + Champions$AwayGoal
scores <- Champions %>% group_by(score) %>% summarise(frequency=n()) %>% arrange(desc(frequency))
scores[1:5,]
# A tibble: 5 x 2
#       score frequency
# <int>     <int>
#   1     1        21
#   2     2        19
#   3     3        18
#   4     4        16
#   5     0        12

#Q2---------
data(cars)
#1
g <- ggplot(cars, aes(x =speed, y= dist)) + geom_point()
#2
g <- g + labs(title="Cars",x="Speed (mpg)",y="Stopping Distance (ft)")
#3
g <- g + geom_point(col="red", pch=17)

#Q3---------
#1
plot.new()
plot.window(xlim=c(0,1), ylim=c(-3,3))
curve(dbeta(x, 2, 6), from=0, to=1, add =T)
curve(dbeta(x, 4, 4), from=0, to=1, add =T)
curve(dbeta(x, 6, 2), from=0, to=1, add =T)
#2
title(expression(f(y)==frac(1,B(a,b))*y^{a-1}*(1-y)^{b-1}))
#3
text(x=0.2,y=dbeta(0.2,2,6),labels=c('(a,b)=(2,6)'),pos=3)
text(x=0.5,y=dbeta(0.5,4,4),labels=c('(a,b)=(4,4)'),pos=3)
text(x=0.8,y=dbeta(0.8,6,2),labels=c('(a,b)=(6,2)'),pos=3)
#4
curve(dbeta(x, 2, 6), from=0, to=1, add =T, col='blue')
curve(dbeta(x, 4, 4), from=0, to=1, add =T, col='red')
curve(dbeta(x, 6, 2), from=0, to=1, add =T, col='green')
legend(x=0.4,y=-0.5,legend=c('2,6','4,4','2,6'),lty=c(1,1), lwd=1.5, bty='n', col=c('blue','red','green'),title='a,b=')

#Q4---------
data(faithful)
#1
faithful <- mutate(faithful, length=(ifelse(eruptions < 3.2, "short", "long")))
#2
g <- ggplot(faithful, aes(length, waiting)) 
g + geom_boxplot()
#3
g <- ggplot(faithful, aes(waiting)) 
g + geom_density(aes(color=length))
faithful %>% group_by(length) %>% summarise(avg_waiting=mean(waiting))
# A tibble: 2 x 2
# length avg_waiting
# <chr>       <dbl>
#   1   long    80.05172
#   2  short    54.64286
#4
# Based on the density curves and boxplots, there is clearly a relationship
# between waiting time and how long the eruptions last for. Long eruptions 
# denoted as longer than 3.2 minutes have an average waiting time of 80.05172 minutes 
# between eruptions and short eruptions have an average waiting time of 54.64286 
# minutes between eruptions.

#Q5---------
load('~/Documents/NYCDSA/Course Work/Week 1/ggplot/Data Visualization Homework/Knicks.rda')
knicks <- data
#1
games = length(knicks)
seasons <- knicks %>% group_by(season) %>% summarise(games=n(),wins=sum(win == 'W'))
seasons$win_ratio = seasons$wins/seasons$games
g <- ggplot(seasons, aes(season, win_ratio))
g + geom_bar(stat='identity')
#2
seasons <- knicks %>% group_by(season, visiting) %>% summarise(games=n(),wins=sum(win == 'W'))
seasons$win_ratio = seasons$wins/seasons$games
seasons = mutate(seasons, visiting=(ifelse(visiting == '0', 'home', 'away')))
g <- ggplot(seasons, aes(season, win_ratio, color=visiting))
g + geom_bar(stat='identity', position = "dodge",aes(fill=visiting))
#3
ggplot(knicks,aes(x=points,color=season)) + geom_histogram(aes(fill=season),bins=40) + facet_wrap(~season)
#4
knicks$points_diff = knicks$points - knicks$opp
opponents <- knicks %>% group_by(opponent) %>% summarise(games=n(),wins=sum(win == 'W'),avg_points_diff=mean(points_diff))
opponents$win_ratio = opponents$wins/opponents$games
# # A tibble: 30 x 5
# opponent games  wins avg_points_diff win_ratio
# <fctr> <int> <int>           <dbl>     <dbl>
#   1          Atlanta Hawks    18     9      -0.6111111 0.5000000
# 2         Boston Celtics    20     4      -6.9000000 0.2000000
# 3      Charlotte Bobcats    18    11       2.7777778 0.6111111
# 4          Chicago Bulls    18     7      -4.1666667 0.3888889
# 5    Cleveland Cavaliers    18     4      -5.3888889 0.2222222
# 6       Dallas Mavericks    10     2     -10.7000000 0.2000000
# 7         Denver Nuggets     9     3      -4.4444444 0.3333333
# 8        Detroit Pistons    18    12       7.1111111 0.6666667
# 9  Golden State Warriors     9     2      -7.7777778 0.2222222
# 10       Houston Rockets     9     1      -9.3333333 0.1111111
# # ... with 20 more rows
ggplot(opponents, aes(win_ratio, avg_points_diff, color=opponent))+ geom_point()
