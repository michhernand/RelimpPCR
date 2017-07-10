#' A Relative Importance PCA Regression Function
#' 
#' This function performs a relative importance PCA regression. It performs PCA and then applys a relative
#' importnace measure on each additional factor. The output shows optimal PCA factor selection for a given
#' regression.
#' @param Y (list/vector): This a list/vector of Y values for the regression.
#' @param X (data frame): This is the input data for the regression.
#' @param relimp_algorithm (string): This is the "type" of relative importance that will be used for measuring
#' raw predictors (not PCA factors).
#' @param max_predictors (int): The maximum number of predictors/factors you want reviewed. Note: For importance
#' measures all predictors/factors will be analyzed for relative importance. Rather, this limits how many
#' predictors/factors are added onto the model to show iteratively increasing R-Suared.
#' @param normalize_data (bool): Whether or not to normalize (subtract mean and divide by standard deviation) before analysis.
#' @param plot (bool): Whether or not to plot the r-squared values. Default is TRUE.
#' @param verbose (bool): Whether or not to include some additional narration around the status of the process.
#' Default is FALSE.
#' @param multicore (bool): Whether or not to use mclapply instead of sapply. Default is TRUE.
#' @return out (list): A list containing all of the below components...
#' @return $pca_factors: the pca factors
#' @return $pca_loadings the pca loadings
#' @return $pca_ordered_factors: the ordered pca factors; These factors should provide optimal model fits in 
#' dimensionality reduction.
#' @return $ordered_predictors: the ordered predictors; These factors provide suboptimal model fits in comparison
#' to ordered pca factors in the case of dimensionality reduction.
#' @return $original_r2: a vector showing the evolution of r-squared when adding one predictor at a time for the
#' original unordered predictors.
#' @return $pca_r2: a vector showing the evolution of r-squared when adding one pca factor at a time for the original
#' unordered pca factors.
#' @return $relimp_r2: a vector showing the evolution of r-squared when adding one predictor at a time for the ordered
#' predictors.
#' @return $relimp_pca_r2: a vector the evolution of r-squared when adding one pca factor at a time for the ordered
#' pca factors.
#' @export

RelimpPCR = function(Y,X,relimp_algorithm="last",max_predictors=0,normalize_data=T,plot=T,verbose=F,multicore=T){
  if(verbose){
    print(paste0(Sys.time()," | Ranking predictors against Y using calc.relimp ",relimp_algorithm))
  }
  
  if(normalize_data == F){
    warning("WARN: Using non-normalized data in PCA can cause sub-optimal results.")
  } else{
    for(z in 1:dim(X)[2]){
      X[,z] = scale(X[,z])
    }
    Y = scale(Y)
  }

  #Ranking
  fit = lm(Y~.,data = data.frame(Y= unlist(Y),X))
  relimp_factors = calc.relimp(fit,type="last")
  ranked_factors = relimp_factors@last.rank
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
  pca_relimp_factors = relaimpo::calc.relimp(pca_fit,type="last")
  pca_ranked_factors = pca_relimp_factors@last.rank
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
    original_r2 = unlist(parallel::mclapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,X[,1:z])))$r.squared))
    relimp_r2 = unlist(parallel::mclapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,ordered_predictors[,1:z])))$r.squared))
    pca_r2 = unlist(parallel::mclapply(X = predictors_range, FUN = function(z) summary(lm(Y~.,data=data.frame(Y=Y,pca_factors[,1:z])))$r.squared))
    pca_relimp_r2 = unlist(parallel::mclapply(X = predictors_range, FUN = function(z) summary(lm(Y~., data = data.frame(Y=Y,pca_ordered_predictors[,1:z])))$r.squared))
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
      
    
  out = list("pca_factors" = pca_factors, "pca_loadings" = pca_loadings, "pca_ordered_factors" = pca_ordered_predictors, "ordered_predictors" = ordered_predictors,
             "original_r2" = original_r2, "pca_r2" = pca_r2, "relimp_pca_r2" = pca_relimp_r2, "relimp_r2" = relimp_r2)
  
  if(verbose){
    print(paste0(Sys.time()," | Process complete"))
  }
  return(out)
}

