rm(list=ls())
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
install.packages("factoextra")
library(factoextra)
install.packages("useful")
library(useful)
install.packages("cluster")
library(cluster)

data = read.csv("C:/Users/jahna/Downloads/goodreads.csv")
data

apply(data, 2, function(x) any(is.na(x)))

sapply(data, class)

data$page_nums <- as.numeric(levels(data$page_nums))[data$page_nums]
data$Average.Rating <- as.numeric(levels(data$Average.Rating))[data$Average.Rating]

apply(data, 2, function(x) any(is.na(x)))

sapply(data, class)

data[is.na(data)] = 0

apply(data, 2, function(x) any(is.na(x)))

sapply(data, class)

data <- data[ -c(1, 5:6, 8) ]
data = data[data$Language.Code=='eng',]
data

df = data[ -c(1:2,4)]
df <- df[order(df$Average.Rating),]
df

scale_M <- scale(df)

set.seed(823)
k_2 <- kmeans(x = scale_M,centers = 2)
print(k_2)
useful::plot.kmeans(x = k_2,data = scale_M)
factoextra::fviz_cluster(object = k_2,data = scale_M)

set.seed(823)
k_3 <- kmeans(x = scale_M,centers = 3)
print(k_3)
useful::plot.kmeans(x = k_3,data = scale_M)
factoextra::fviz_cluster(object = k_3,data = scale_M)

set.seed(823)
k_4 <- kmeans(x = scale_M,centers = 4)
print(k_4)
useful::plot.kmeans(x = k_4,data = scale_M)
factoextra::fviz_cluster(object = k_4,data = scale_M)

set.seed(823)
k_5 <- kmeans(x = scale_M,centers = 5)
useful::plot.kmeans(x = k_5,data = scale_M)
factoextra::fviz_cluster(object = k_5,data = scale_M)

set.seed(823)
factoextra::fviz_nbclust( x = scale_M,FUNcluster = kmeans,method = "wss")

clara_M <- cluster::clara(x = scale_M,k=5)
plot(clara_M)
print(clara_M)
set.seed(823)
factoextra::fviz_nbclust( x = scale_M,FUNcluster = clara,method = "wss")

fanny_M <- cluster::fanny(x = scale_M,k = 5)
plot(fanny_M)
print(fanny_M)
factoextra::fviz_nbclust( x = scale_M,FUNcluster = kmeans,method = "wss")

pam_M <- cluster::pam(x=scale_M, k = 5)
plot(pam_M)
print(pam_M)

dist_M <- dist(scale_M[1:100, ])
dist_M

dist_M <- dist(scale_M[1:100, ])
heatmap(
  x = as.matrix(dist_M),
  col = viridis::viridis(256)
)

factoextra::fviz_dist(dist_M)

qgraph::qgraph(
  input = 1/dist_M,
  layout="spring",
  minimum = 0.3
)

hclust_M <- hclust(dist_M)
plot(hclust_M)

cutree_M <- cutree(
  tree = hclust_M,
  k = 5
)
cutree_M

silhouette_M <- cluster::silhouette(
  x = cutree_M,
  dist = dist_M
)
plot(silhouette_M)


v_dist <- c("manhattan","euclidean")
list_dist <- lapply(
  X = v_dist,
  FUN = function(distance_method) dist(
    x = scale_M,
    method = distance_method
  ))

names(list_dist) <- v_dist
v_hclust <- c("ward.D","single")

list_hclust <- list()
for(j in v_dist) for(k in v_hclust) list_hclust[[j]][[k]] <- hclust(
  d = list_dist[[j]],
  method = k
)
par(
  mfrow = c(length(v_dist),length(v_hclust)),
  mar = c(0,0,0,0),
  mai = c(0,0,0,0),
  oma = c(0,0,0,0)
)
for(j in v_dist) for(k in v_hclust) plot(
  x = list_hclust[[j]][[k]],
  labels = FALSE,
  axes = FALSE,
  main = paste("\n",j,"\n",k)
)


for(j in v_dist) for(k in v_hclust) list_hclust[[j]][[k]]$height <- rank(list_hclust[[j]][[k]]$height)
par(
  mfrow = c(length(v_dist),length(v_hclust)),
  mar = c(0,0,0,0),
  mai = c(0,0,0,0),
  oma = c(0,0,0,0)
)
for(j in v_dist) for(k in v_hclust) plot(
  x = list_hclust[[j]][[k]],
  labels = FALSE,
  axes = FALSE,
  main = paste("\n",j,"\n",k)
)


agnes_M <- cluster::agnes(scale_M[1:100, ])
plot(agnes_M)

diana_M <- cluster::diana(scale_M[1:100, ])
plot(diana_M)
























