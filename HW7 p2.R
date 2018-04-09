#Installing necessary packages and setting up working directory
library(jpeg)
library(grid)

rm(list=ls())
setwd("C:/Users/Bajes01842/Google Drive/CS/Machine Learning/HW7/RPacks")
#setwd("C:/Users/Behrooz/Google Drive/CS/Machine Learning/HW7/RPacks")

img <- readJPEG("RobertMixed03.jpg")
img255 <- img*255
img.r <-  matrix(img255[,,1],nrow = 480,ncol=640)
img.g <-  matrix(img255[,,2],nrow = 480,ncol=640)
img.b <-  matrix(img255[,,3],nrow = 480,ncol=640)
n <- nrow(img.r)*ncol(img.r)
k <- 50
d <- 3

#image(t(img.r)[,nrow(img.r):1])
#image(t(img.g)[,nrow(img.g):1])
#image(t(img.b)[,nrow(img.b):1])

img.all <- matrix(0,nrow=n,ncol=3)
for(i in 1:nrow(img.r)){
  for(j in 1:ncol(img.r)){
    img.all[640*(i-1)+j,] <- img255[i,j,]
  }
}


Cluster.all <- kmeans(img.all, k, nstart = 20, iter.max=30)
#plot(img.all[1:1000,1:2], col = Cluster.all$cluster)
#points(Cluster.all$centers, col = 1:10, pch = 8, cex = 2)
mean.k <- Cluster.all[["centers"]]
mean.new <- matrix(0,nrow = k, ncol = d)

P.new <- matrix(0.1,ncol = k,nrow = 1)
mean.new <- mean.k


#repeat{
for(t in 1:10){
  
  H1 <- (img.all*img.all) %*% (matrix(1,nrow = d,ncol = k))
  H2 <- matrix(1,nrow = n,ncol = d) %*% t(mean.new*mean.new)
  H3 <- img.all %*% t(mean.new)
  H <- (H1+H2-2*H3)
  for(i in 1:n){
    H [i,] <- H [i,]- min(apply((matrix(1,ncol=1,nrow=k) %*% img.all[i,]-mean.new)^2,1,sum))
  }
  H <- H*-0.5
  
  E <- H
  for (l in 1:n){
    E[l,] <- exp(H[l,])*P.new
  }
  W <- E
  for (l in 1:n){
    W[l,] <- E[l,]/sum(E[l,])
  }
  

  P.old <- P.new
  P.new <- apply(W,2,sum) / n
  for (r in 1:d){
    mean.new[,r] <- (t(img.all[,r]) %*% W )/ (apply(W,2,sum))
  }
  
  
  print(mean(P.old - P.new))
  #if(mean(P.old - P.new) < 0.0000000001){
  #  break}
}


img.all.new <- img.all
for (l in 1:n){
  img.all.new[l,] <- mean.new[which.max(W[l,]),]
}

r <- matrix (img.all.new[,1],ncol = 640, nrow = 480, byrow = T)
g <- matrix (img.all.new[,2],ncol = 640, nrow = 480, byrow = T)
b <- matrix (img.all.new[,3],ncol = 640, nrow = 480, byrow = T)
col <- rgb(r, g, b, maxColorValue = 255)
dim(col) <- dim(r)
grid.raster(col, interpolate=FALSE)

#image(t(img.r.new)[,nrow(img.r.new):1])
