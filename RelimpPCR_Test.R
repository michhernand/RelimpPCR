require(RUnit)

source(RelimpPCR.R)

test.RelimpPCR <- function() {
  set.seed(8394756)
  Epsilon<-rnorm(500,0,1)
  X<-rnorm(500*500,0,2)
  dim(X)<-c(500,500)
  colnames(X)<-paste0("X",1:500)
  slopesSet<-runif(500,1,3)
  Y<-sapply(2:500,function(z) 1+X[,1:z]%*%slopesSet[1:z]+Epsilon)
  
  checkEquals(RelimpPCR(Y[,250],X[,1:250])$relimp_r2[1],0.008512093,multicore=F)
  checkEquals(RelimpPCR(Y[,250],X[,1:250])$relimp_pca_r2[1],0.08319307,multicore=F)
  checkEquals(RelimpPCR(Y[,250],X[,1:250])$pca_r2[1],0.0003308157,multicore=F)
  checkEquals(RelimpPCR(Y[,250],X[,1:250])$original_r2[1],0.003703125,multicore=F)
}
