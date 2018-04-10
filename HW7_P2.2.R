#Installing necessary packages and setting up working directory
library(jpeg)
library(grid)
rm(list=ls())
#setwd("C:/Users/Bajes01842/Google Drive/CS/Machine Learning/HW7/RPacks")
setwd("C:/Users/Behrooz/Google Drive/CS/Machine Learning/HW7/RPacks")


#Reading in the image and seprating the RGB digits
img <- readJPEG("smallsunset.jpg")
img255 <- img*255
img.r <-  matrix(img255[,,1],nrow = nrow(img255[,,1]),ncol=ncol(img255[,,1]))
img.g <-  matrix(img255[,,2],nrow = nrow(img255[,,1]),ncol=ncol(img255[,,1]))
img.b <-  matrix(img255[,,3],nrow = nrow(img255[,,1]),ncol=ncol(img255[,,1]))


#Setting up the constants for number of pixels, segments and colors
n <- nrow(img.r)*ncol(img.r)
k <- c(10,20,50)
d <- 3
Clus.Num <- 2


#Setting up the RGB array for each pixel of an image as a row
img.all <- matrix(0,nrow=n,ncol=d)
for(i in 1:nrow(img.r)){
  for(j in 1:ncol(img.r)){
    img.all[ncol(img.r)*(i-1)+j,] <- img255[i,j,]
  }
}


#Obtaining the k-mean cluster centers as the starting point for segments mean values
Cluster.all <- kmeans(img.all, k[Clus.Num], nstart = 20, iter.max=30)
mean.k <- Cluster.all[["centers"]]


#Performing the EM loop for various starting points
for (Mean.Change in 1:5){

  
  #Generating various starting points  
  mean.new <- mean.k+(((3-Mean.Change)*0.45)*mean.k)
  P.new <- matrix(1/(k[Clus.Num]),ncol = k[Clus.Num],nrow = 1)
  
  #Performing EM loop for each starting point, loop breaks as convergence criteria is checked
  repeat{
    
    #Performing the E step
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
    
    
    #Performing the M step
    P.old <- P.new
    P.new <- apply(W,2,sum) / n
    for (r in 1:d){
      mean.new[,r] <- (t(img.all[,r]) %*% W )/ (apply(W,2,sum))
    }
    
    
    print(mean((P.new - P.old)^2))
    if(mean((P.new - P.old)) < 0.0000001){
      break
    }
  }
  
  
  #Replacing the result image pixels with the mean RGB of segments they belong to
  img.all.new <- img.all
  for (l in 1:n){
    img.all.new[l,] <- mean.new[which.max(W[l,]),]
  }
  
  
  #Generating the result image and writting to the file
  r <- matrix (img.all.new[,1],ncol = ncol(img.r), nrow = nrow(img.r), byrow = T)
  g <- matrix (img.all.new[,2],ncol = ncol(img.r), nrow = nrow(img.r), byrow = T)
  b <- matrix (img.all.new[,3],ncol = ncol(img.r), nrow = nrow(img.r), byrow = T)
  
  col.2 <- array(0, dim = c(nrow(img.r), ncol(img.r), 3))
  col.2 [,,1] <- r
  col.2 [,,2] <- g
  col.2 [,,3] <- b
  col.2 <- col.2/255
  
  addr <- paste('C:/Users/Behrooz/Google Drive/CS/Machine Learning/HW7/RPacks')
  writeJPEG(col.2, target = paste(addr,paste(Mean.Change,'.jpg'),sep='/'))
}


