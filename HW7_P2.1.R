#Installing necessary packages and setting up working directory
library(jpeg)
library(grid)

rm(list=ls())
setwd("C:/Users/Bajes01842/Google Drive/CS/Machine Learning/HW7/RPacks")
#setwd("C:/Users/Behrooz/Google Drive/CS/Machine Learning/HW7/RPacks")

img <- readJPEG("RobertMixed03.jpg")
#img <- readJPEG("smallstrelitzia.jpg")
#img <- readJPEG("smallsunset.jpg")


img255 <- img*255
img.r <-  matrix(img255[,,1],nrow = nrow(img255[,,1]),ncol=ncol(img255[,,1]))
img.g <-  matrix(img255[,,2],nrow = nrow(img255[,,1]),ncol=ncol(img255[,,1]))
img.b <-  matrix(img255[,,3],nrow = nrow(img255[,,1]),ncol=ncol(img255[,,1]))

n <- nrow(img.r)*ncol(img.r)
k <- c(10,20,50)
d <- 3
img.all <- matrix(0,nrow=n,ncol=d)
for(i in 1:nrow(img.r)){
  for(j in 1:ncol(img.r)){
    img.all[ncol(img.r)*(i-1)+j,] <- img255[i,j,]
  }
}

for (Clus.Num in 1:3){
  

  #image(t(img.r)[,nrow(img.r):1])
  #image(t(img.g)[,nrow(img.g):1])
  #image(t(img.b)[,nrow(img.b):1])

  


  Cluster.all <- kmeans(img.all, k[Clus.Num], nstart = 20, iter.max=30)
  #plot(img.all[1:1000,1:2], col = Cluster.all$cluster)
  #points(Cluster.all$centers, col = 1:10, pch = 8, cex = 2)
  mean.k <- Cluster.all[["centers"]]
  mean.new <- mean.k

  P.new <- matrix(1/k[Clus.Num],ncol = k[Clus.Num],nrow = 1)



  repeat{
  #for(t in 1:10){
  
    H1 <- (img.all*img.all) %*% (matrix(1,nrow = d,ncol = k[Clus.Num]))
    H2 <- matrix(1,nrow = n,ncol = d) %*% t(mean.new*mean.new)
    H3 <- img.all %*% t(mean.new)
    H <- (H1+H2-2*H3)
    for(i in 1:n){
      H [i,] <- H [i,]- min(apply((matrix(1,ncol=1,nrow=k[Clus.Num]) %*% img.all[i,]-mean.new)^2,1,sum))
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
    if(mean(P.old - P.new) < 0.0000000001){
      break
    }
  }


  img.all.new <- img.all
  for (l in 1:n){
    img.all.new[l,] <- mean.new[which.max(W[l,]),]
  }

  r <- matrix (img.all.new[,1],ncol = ncol(img.r), nrow = nrow(img.r), byrow = T)
  g <- matrix (img.all.new[,2],ncol = ncol(img.r), nrow = nrow(img.r), byrow = T)
  b <- matrix (img.all.new[,3],ncol = ncol(img.r), nrow = nrow(img.r), byrow = T)
  
  
  col.2 <- array(0, dim = c(nrow(img.r), ncol(img.r), 3))
  col.2 [,,1] <- r
  col.2 [,,2] <- g
  col.2 [,,3] <- b
  col.2 <- col.2/255
  
  
  addr <- paste('C:/Users/Bajes01842/Google Drive/CS/Machine Learning/HW7/RPacks')
  writeJPEG(col.2, target = paste(addr,paste(k[Clus.Num],'.jpg'),sep='/'))
  #image(t(img.r.new)[,nrow(img.r.new):1])
  
}


