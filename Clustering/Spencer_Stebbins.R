#--------------------------------------------------------------
# Question #1: K-Means  
#--------------------------------------------------------------
library(drs)

# 1. Use the following commands to read the data into your workspace appropriately and scale the variables:
protein <- read.table('~/Documents/NYCDSA/Course Work/Clustering/Cluster Analysis Homework/[08] Protein.txt', sep = '\t', header = TRUE)
protein.scaled <- as.data.frame(scale(protein[, -1]))
rownames(protein.scaled) <- protein$Country
open_sesame(protein)
uni(protein)
open_sesame(protein.scaled)
uni(protein.scaled)

# 2. Create and interpret a scree-plot for the within-cluster variance for various values of K used in the K-means algorithm.
wssplot = function(data, nc = 15, seed = 0) {
    wss = (nrow(data) - 1) * sum(apply(data, 2, var))
    for (i in 2:nc) {
        set.seed(seed)
        wss[i] = sum(kmeans(data, centers = i, iter.max = 100, nstart = 100)$withinss)
    }  # withinss = within cluster sum of squares
    plot(1:nc, wss, type = "b",
         xlab = "Number of Clusters",
         ylab = "Within-Cluster Variance",
         main = "Scree Plot for the K-Means Procedure")
}
# a. Why might this graph indicate that K-means is not truly appropriate to model the data?

wssplot(protein.scaled)
# There is no point where the difference of the within-cluster variance 
# drastically drops and then levels off. This means that if groups exist, they
# might not be spherical in shape

# 3. Create and store 5 different K-means solutions that run the algorithm only 1 time each. (N B: Use  set.seed(0) so your results will be reproducible.)
set.seed(0)
km.protein.1 = kmeans(protein.scaled, centers = 10)
km.protein.2 = kmeans(protein.scaled, centers = 10)
km.protein.3 = kmeans(protein.scaled, centers = 10)
km.protein.4 = kmeans(protein.scaled, centers = 10)
km.protein.5 = kmeans(protein.scaled, centers = 10)

# 4. Create and store 1 K-means solution that was selected from running the algorithm 100 separate times. (N B: Use  set.seed(0) so your results will be reproducible.)
set.seed(0)
km.protein.100x <- kmeans(protein.scaled, centers = 10, nstart = 100)
# 5. Plot the 6 different solutions from part 3 and 4 with:
# a. Cereals on the x-axis.
# b. RedMeat on the y-axis.
# c. Colors for the different cluster assignments.
# d. Labels for the total within-cluster variances.
par(mfrow = c(2, 3))
plot(protein.scaled$Cereals, protein.scaled$RedMeat, 
     col = km.protein.1$cluster,
     main = paste('Single K-Means Attempt 1:\n WCV:',
                  round(km.protein.1$tot.withinss), 4))
plot(protein.scaled$Cereals, protein.scaled$RedMeat, 
     col = km.protein.2$cluster,
     main = paste('Single K-Means Attempt 2:\n WCV:',
                  round(km.protein.2$tot.withinss), 4))
plot(protein.scaled$Cereals, protein.scaled$RedMeat, 
     col = km.protein.3$cluster,
     main = paste('Single K-Means Attempt 3:\n WCV:',
                  round(km.protein.3$tot.withinss), 4))
plot(protein.scaled$Cereals, protein.scaled$RedMeat, 
     col = km.protein.4$cluster,
     main = paste('Single K-Means Attempt 4:\n WCV:',
                  round(km.protein.4$tot.withinss), 4))
plot(protein.scaled$Cereals, protein.scaled$RedMeat, 
     col = km.protein.5$cluster,
     main = paste('Single K-Means Attempt 5:\n WCV:',
                  round(km.protein.5$tot.withinss), 4))
plot(protein.scaled$Cereals, protein.scaled$RedMeat, 
     col = km.protein.100x$cluster,
     main = paste('Single K-Means Attempt 100x:\n WCV:',
                  round(km.protein.100x$tot.withinss), 4))

# 6. Plot the solution from part 4 with: 
# a. Cereals on the x-axis.
# b. RedMeat on the y-axis.
# c. A label for the total within-cluster variance.
# d. Points for the centroids of each cluster.
# e. A horizontal line at 0.
# f. A vertical line at 0.
par(mfrow = c(1, 1))
plot(km.protein.100x$centers[, 4],
     km.protein.100x$centers[, 2],
     main = paste('Single K-Means Attempt 100x:\n WCV:',
                  round(km.protein.100x$tot.withinss, 4)),
     pch = 16, col = "blue")
abline(h=0)
abline(v=0)
text(protein.scaled$Cereals, protein.scaled$RedMeat,
     col = km.protein.100x$cluster,
     labels = rownames(protein.scaled))

#--------------------------------------------------------------
# Question #2: Hierarchical Clustering
#--------------------------------------------------------------

library(flexclust)

# 1.Calculate and store pairwise distances for each observation in the dataset.
d = dist(protein.scaled)

# 2.Fit hierarchical clustering solutions using single, complete, and average linkage.
clust.single <- hclust(d, method = 'single')
clust.complete <- hclust(d, method = 'complete')
clust.avg <- hclust(d, method = 'average')

# 3. Visualize the dendrograms created in part 2.
par(mfrow = c(1, 3))
plot(clust.single, hang = -1, main = 'Dendrogram of Single Linkage')
plot(clust.complete, hang = -1, main = 'Dendrogram of Complete Linkage')
plot(clust.avg, hang = -1, main = 'Dendrogram of Average Linkage')
# a. Give an argument as to why single linkage might not be good to use.
# With single linkage, it looks like it's chaining exists

# b. Give an argument as to why complete linkage might be good to use.
# Complete linkage looks good to use because the break points seem evenly distrubuted across the dendogram

# 4. Cut your complete linkage tree into 2 groups.
clust.cut2 <- cutree(clust.complete, k = 2)
clust.cut2
table(clust.cut2)
# a. Visualize the solution overlaid on top of the dendrogram.
par(mfrow = c(1, 1))
plot(clust.complete, hang = -1, main = "Dendrogram of Complete Linkage\n2 Clusters")
rect.hclust(clust.complete, k = 2)
# b. Interpret the clusters by aggregating across the median.
aggregate(protein.scaled, by = list(cluster = clust.cut2), median)
# Countries generally in nothern europe eat more protien than thoe of souther europe.

# 5. Cut your complete linkage tree into 5 groups.
clust.cut5 <- cutree(clust.complete, k = 5)
clust.cut5
table(clust.cut5)
# a.Visualize the solution overlaid on top of the dendrogram.
par(mfrow = c(1, 1))
plot(clust.complete, hang = -1, main = "Dendrogram of Complete Linkage\n5 Clusters")
rect.hclust(clust.complete, k = 5)
# b.Interpret the clusters by aggregating across the median.
aggregate(protein.scaled, by = list(cluster = clust.cut5), median)
# There are five groups: Eastern Europe, Scandanavia, Western Europe, Iberia,
# and Southern/Eastern Europe. 
