RelimpPCR = function(Y,X,relimp_algorithm="last",max_predictors=0,plot=T,verbose=F,multicore=T){
  suppressMessages(require(relaimpo))
  suppressMessages(require(parallel))
  if(verbose){
    print(paste0(Sys.time()," | Ranking predictors against Y using calc.relimp ",relimp_algorithm))
  }
  
  #Ranking
  fit = lm(Y~.,data = data.frame(Y= unlist(Y),X))
  ranked_factors = calc.relimp(fit,type="last")
  ranked_factors = ranked_factors@last.rank
  ordered_predictors = X[,order(ranked_factors)]
  
  if(verbose){
    print(paste0(Sys.time()," | Running PCA"))
  }
  
  #PCA
  pca = prcomp(X)
  pca_factors = pca$x
  pca_loadings = pca$rotation
  
  if(verbose){
    print(paste0(Sys.time()," | Ranking PCA factors against Y using calc.relimp"))
  }
  
  #PCA Ranking
  pca_fit = lm(Y~.,data = data.frame(Y = unlist(Y),pca_factors))
  pca_ranked_factors = calc.relimp(pca_fit,type="last")
  pca_ranked_factors = pca_ranked_factors@last.rank
  pca_ordered_predictors = pca_factors[,order(pca_ranked_factors)]

  if(max_predictors > dim(X)[2]){
    stop("ERROR: You cannot have 'max_predictors' be greater than the total number of predictors in your data set.")
  }
    
  if(max_predictors <= 0){
    predictors_range = 1:dim(X)[2]
  } else {
    predictors_range = 1:max_predictors
  }
  
  if(verbose){
    print(paste0(Sys.time()," | Iteratively adding predictors according to order/ranking"))
  }

  if(multicore==T){
    original_r2 = unlist(mclapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,X[,1:z])))$r.squared))
    relimp_r2 = unlist(mclapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,ordered_predictors[,1:z])))$r.squared))
    pca_r2 = unlist(mclapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,pca_factors[,1:z])))$r.squared))
    pca_relimp_r2 = unlist(mclapply(X = predictors_range, FUN = function(z) summary(lm(Y~., data = data.frame(Y=Y,pca_ordered_predictors[,1:z])))$r.squared))
  } else {
    original_r2 = sapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,X[,1:z])))$r.squared)
    relimp_r2 = sapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,ordered_predictors[,1:z])))$r.squared)
    pca_r2 = sapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,pca_factors[,1:z])))$r.squared)
    pca_relimp_r2 = sapply(X = predictors_range, FUN = function(z) summary(lm(Y~., data = data.frame(Y=Y,pca_ordered_predictors[,1:z])))$r.squared)
  }
  
  if(plot==T){
    matplot(predictors_range,cbind(original_r2,pca_r2,relimp_r2,pca_relimp_r2),type="l",lty=1,lwd=2,col=c("black","red","green","blue"),
            main="Improvement of Fit with Number of Predictors",
            xlab="Number of Predictors",ylab="Determination Coefficient")
    legend("bottomright",legend=c("Original","PCA","Relimp","PCA w/ Relimp"),lty=1,lwd=2,col=c("black","red","green","blue"))
  }
      
    
  out = list("pca_factors" = pca_factors, "pca_loadings" = pca_loadings, "pca_ordered_predictors" = pca_ordered_predictors, "ordered_predictors" = ordered_predictors,
             "original_r2" = original_r2, "pca_r2" = pca_r2, "relimp_pca_r2" = pca_relimp_r2, "relimp_r2" = relimp_r2)
  
  if(verbose){
    print(paste0(Sys.time()," | Process complete"))
  }
  return(out)
}

