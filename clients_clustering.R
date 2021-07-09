#importing essential packages
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(plotrix)) install.packages("plotrix", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")

setwd("D:/GitHub/clients_clustering/clients_dataset")

customer_data = read.csv("Mall_Customers.csv") #reading data from file

#analysis
str(customer_data) #structure of the data frame

names(customer_data) #row titles only

head(customer_data) #the first six rows

#dataset summary
summary(customer_data) # age summary

#barplot of gender distribution
a = table(customer_data$Gender) #fetch from Gender column only
barplot(a, main="Gender Comparision",
        ylab = "Count",
        xlab = "Gender",
        col = c("#009999", "#0000FF"),
        legend = rownames(a))

#piechart showing the gender ratios
pct = round(a/sum(a)*100)
lbs = paste(c("Female", "Male"), " ", pct, "%", sep = " ")
pie3D(a,labels=lbs,
      main = "Ratio of Female and Male")

#age distribution
summary(customer_data$Age) # age summary of our data

#the histogram
hist(customer_data$Age,
     col = "grey",
     main = "Count of Age Class",
     xlab = "Age Class",
     ylab = "Frequency",
     labels = TRUE#adding frequency to individual bars
     ) 

summary(customer_data$Annual.Income..k..) # summary of the income data

#annual income histogram
hist(customer_data$Annual.Income..k..,
     col = "grey",
     main = " Annual Income Distribution",
     xlab = "Annual Income Class",
     ylab = "Frequency",
     labels = TRUE
     )

#the density plot
plot(density(customer_data$Annual.Income..k..),
     col = "blue",
     main = "Annual Income Distribution",
     xlab = "Annual Income Class",
     ylab = "Density") 
#filled density plot
polygon(density(customer_data$Annual.Income..k..),
        col="grey", border = "blue") 

#spending score analysis
summary(customer_data$Spending.Score..1.100.) #the summary

hist(customer_data$Spending.Score..1.100.,
     main = "Spending Score",
     xlab = "Spending Score Class",
     ylab = "Frequency",
     col = "grey",
     labels = TRUE)

#The elbow method
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5], k, iter.max=100, nstart=100, algorithm = "Lloyd" )$tot.withinss
}

k.values <- 1:10 #Number of clusters K

iss_values <- map_dbl(k.values, iss) #Total intra-clusters sum of squares

plot(k.values, iss_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total intra-clusters sum of squares",
     main = "The Elbow Plot")

#Silhouette Method
k2 <- kmeans(customer_data[, 3:5], 2, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s2 <- plot(silhouette(k2$cluster, dist(customer_data[, 3:5], "euclidean")))

k3 <- kmeans(customer_data[, 3:5], 3, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s3 <- plot(silhouette(k3$cluster, dist(customer_data[, 3:5], "euclidean")))

k4 <- kmeans(customer_data[, 3:5], 4, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s4 <- plot(silhouette(k4$cluster, dist(customer_data[, 3:5], "euclidean")))

k5 <- kmeans(customer_data[, 3:5], 5, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s5 <- plot(silhouette(k5$cluster, dist(customer_data[, 3:5], "euclidean")))

k6 <- kmeans(customer_data[, 3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s6 <- plot(silhouette(k6$cluster, dist(customer_data[, 3:5], "euclidean")))

k7 <- kmeans(customer_data[, 3:5], 7, iter.max = 100,nstart = 50,algorithm = "Lloyd")
s7 <- plot(silhouette(k7$cluster, dist(customer_data[, 3:5], "euclidean")))

k8 <- kmeans(customer_data[, 3:5], 8, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s8 <- plot(silhouette(k8$cluster, dist(customer_data[, 3:5], "euclidean")))

k9 <- kmeans(customer_data[, 3:5], 9, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s9 <- plot(silhouette(k9$cluster, dist(customer_data[, 3:5], "euclidean")))

k10 <- kmeans(customer_data[, 3:5], 10, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s10 <- plot(silhouette(k10$cluster, dist(customer_data[, 3:5], "euclidean")))

#visualizing the optimal number of clusters
library(NbClust)
library(factoextra)

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")


#the Gap statistic method using clusGap() function
set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap) #the plot

#output of our optimal cluster
k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

# visualizing the clusters
pcclust = prcomp(customer_data[, 3:5], scale = FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[, 1:2]

#the plot
set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("K-means Clustering")

#ploting k-means against the clusters
kCols = function(vec){
  cols = rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])
  }

digCluster <- k6$cluster; 
dignm <- as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], #the principle component algorithm
     col = kCols(digCluster), pch = 19, xlab = "K-means", ylab = "classes",
     main = "Cluster k-means")
legend("bottomright", unique(dignm), fill=unique(kCols(digCluster))) 


  
