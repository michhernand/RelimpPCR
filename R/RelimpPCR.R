RelimpPCR = function(Y,X,type="relimp",verbose=F){
  suppressMessages(require(relaimpo))
  
  pca = prcomp(X)
  
  pca_factors = pca$x
  pca_loadings = pca$rotation
  
  fit = lm(Y~.,data = data.frame(Y = unlist(Y),pca_factors))
  
  ranked_factors = calc.relimp(fit,type="last")
  ranked_factors = ranked_factors@last.rank
  ordered_predictors = X[,order(ranked_factors)]
  
  return(ordered_predictors)
}

