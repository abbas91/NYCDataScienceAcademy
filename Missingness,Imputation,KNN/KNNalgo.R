#Inspecting the Voronoi tesselation for the complete observations in the iris
#dataset.
library(deldir) #Load the Delaunay triangulation and Dirichelet tesselation library.
library(mnormt)
library("scatterplot3d")

data <- iris[iris$Species == 'setosa' | iris$Species == 'versicolor',]
data  <- data[!is.na(data$Species),] 

# 
# col.vec = c(rep("red", 50), rep("green", 50))
#             
# info = deldir(data$Sepal.Length,
#               data$Sepal.Width)
# plot.tile.list(tile.list(info), fillcol = col.vec,main = "Iris Voronoi Tessellation\nDecision Boundaries")
# 

x <- data

v <- x[x$Species == 'versicolor',]

cov.mtx <- cov(v[, names(v)[1:2]])
mean <- sapply(v[, names(v)[1:2]], mean)
v$pdf <- dmnorm(v[, names(v)[1:2]], mean = mean, varcov = cov.mtx)

scatterplot3d(cbind(v[, names(v)[1:2]], dmnorm(v[, names(v)[1:2]], mean = mean, varcov = cov.mtx)))

plot(v$Sepal.Length, v$Sepal.Width)            
              
# s <- x[x$Species == 'setosa',]
# 
# cov.mtx <- cov(s[, names(s)[1:2]])
# mean <- sapply(s[, names(s)[1:2]], mean)
# s$pdf <- dmnorm(s[, names(s)[1:2]], mean = mean, varcov = cov.mtx)
# 
# scatterplot3d(cbind(s[, names(s)[1:2]], 
#                     dmnorm(s[, names(s)[1:2]], mean = mean, varcov = cov.mtx)))









train <- rbind(iris3[1:25,1:2,1],
               iris3[1:25,1:2,2],
               iris3[1:25,1:2,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

library(MASS)

test <- expand.grid(x=seq(min(train[,1]-1), max(train[,1]+1),
                          by=0.1),
                    y=seq(min(train[,2]-1), max(train[,2]+1), 
                          by=0.1))

library(class)
classif <- knn(train, test, cl, k = 3, prob=TRUE)
prob <- attr(classif, "prob")

library(dplyr)

dataf <- bind_rows(mutate(test,
                          prob=prob,
                          cls="c",
                          prob_cls=ifelse(classif==cls,
                                          1, 0)),
                   mutate(test,
                          prob=prob,
                          cls="v",
                          prob_cls=ifelse(classif==cls,
                                          1, 0)),
                   mutate(test,
                          prob=prob,
                          cls="s",
                          prob_cls=ifelse(classif==cls,
                                          1, 0)))

library(ggplot2)
ggplot(dataf) +
    # geom_point(aes(x=x, y=y, col=cls),
    #            data = mutate(test, cls=classif),
    #            size=1.2) + 
    geom_contour(aes(x=x, y=y, z=prob_cls, group=cls, color=cls),
                 bins=2,
                 data=dataf) +
    geom_point(aes(x=x, y=y, col=cls),
               size=3,
               data=data.frame(x=train[,1], y=train[,2], cls=cl))