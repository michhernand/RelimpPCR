RelimpPCR = function(Y,X,type="relimp",max_predictors=0,plot=T,verbose=F,multicore=T){
  suppressMessages(require(relaimpo))
  suppressMessages(require(parallel))
  
  pca = prcomp(X)
  
  pca_factors = pca$x
  pca_loadings = pca$rotation
  
  pca_fit = lm(Y~.,data = data.frame(Y = unlist(Y),pca_factors))
  
  ranked_factors = calc.relimp(pca_fit,type="last")
  ranked_factors = ranked_factors@last.rank
  ordered_predictors = X[,order(ranked_factors)]
  
  if(max_predictors == 0){
    if(multicore==T){
      original_r2 = unlist(mclapply(X = 1:dim(X)[2], FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,X[,1:z])))$r.squared))
      pca_r2 = unlist(mclapply(X = 1:dim(X)[2], FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,pca_factors[,1:z])))$r.squared))
      relimp_r2 = unlist(mclapply(X = 1:dim(X)[2], FUN = function(z) summary(lm(Y~., data = data.frame(Y=Y,ordered_predictors[,1:z])))$r.squared))
    } else {
      original_r2 = sapply(X = 1:dim(X)[2], FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,X[,1:z])))$r.squared)
      pca_r2 = sapply(X = 1:dim(X)[2], FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,pca_factors[,1:z])))$r.squared)
      relimp_r2 = sapply(X = 1:dim(X)[2], FUN = function(z) summary(lm(Y~., data = data.frame(Y=Y,ordered_predictors[,1:z])))$r.squared)
    }
    
    if(plot==T){
      matplot(1:dim(X)[2],cbind(original_r2,pca_r2,relimp_r2),type="l",lty=1,lwd=2,col=c("black","red","blue"),
              main="Improvement of Fit with Number of Predictors",
              xlab="Number of Predictors",ylab="Determination Coefficient")
      legend("bottomright",legend=c("Original","PCA","PCA w/ Relimp"),lty=1,lwd=2,col=c("black","red","blue"))
      
    }
    
  } else {
    
    #Check if max_predictors is a valid #.
    if(max_predictors > dim(X)[2]){
      stop("ERROR: You cannot have 'max_predictors' be greater than the total number of predictors in your data set.")
    }
    
    if(multicore==T){
      original_r2 = unlist(mclapply(X = 1:max_predictors, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,X[,1:z])))$r.squared))
      pca_r2 = unlist(mclapply(X = 1:max_predictors, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,pca_factors[,1:z])))$r.squared))
      relimp_r2 = unlist(mclapply(X = 1:max_predictors, FUN = function(z) summary(lm(Y~., data = data.frame(Y=Y,ordered_predictors[,1:z])))$r.squared))
    } else {
      original_r2 = sapply(X = 1:max_predictors, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,X[,1:z])))$r.squared)
      pca_r2 = sapply(X = 1:max_predictors, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,pca_factors[,1:z])))$r.squared)
      relimp_r2 = sapply(X = 1:max_predictors, FUN = function(z) summary(lm(Y~., data = data.frame(Y=Y,ordered_predictors[,1:z])))$r.squared)
    }
    
    if(plot==T){
      matplot(1:max_predictors,cbind(original_r2,pca_r2,relimp_r2),type="l",lty=1,lwd=2,col=c("black","red","blue"),
              main="Improvement of Fit with Number of Predictors",
              xlab="Number of Predictors",ylab="Determination Coefficient")
      legend("bottomright",legend=c("Original","PCA","PCA w/ Relimp"),lty=1,lwd=2,col=c("black","red","blue"))
      
    }
  }

  
  out = list("pca_factors" = pca_factors, "pca_loadings" = pca_loadings, "ordered_predictors" = ordered_predictors,
             "original_r2" = original_r2, "pca_r2" = pca_r2, "relimp_r2" = relimp_r2)
  
  return(out)
}

