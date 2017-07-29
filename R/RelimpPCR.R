#' A Relative Importance PCA Regression Function
#' 
#' This function performs a relative importance PCA regression. It performs PCA and then applys a relative
#' importnace measure on each additional factor. The output shows optimal PCA factor selection for a given
#' regression.
#' @param Y (list/vector): This a list/vector of Y values for the regression.
#' @param X (data frame): This is the input data for the regression.
#' @param target_r2 (float 0-1): The algorithm will attempt to return to you the simplest model (i.e. with fewest predictors) that satisfies
#' your target_r2 value; If no model satisfies this condition, then the full model (with all predictors) will be returned.
#' @param r2_type (string "train" or "test): This defines which r-squared value the target_r2 argument will evaluate.
#' @param validation_split (float 0-1): This determines how much of your data set will be in the train data set. The remainder will be
#' allocated to the test data set. If set to 1, train and test samples will be identical.
#' @param relimp_algorithm (string): This is the "type" of relative importance that will be used for measuring
#' raw predictors (not PCA factors).
#' @param max_predictors (int): The maximum number of predictors/factors you want reviewed. Note: For importance
#' measures all predictors/factors will be analyzed for relative importance. Rather, this limits how many
#' predictors/factors are added onto the model to show iteratively increasing R-Suared.
#' @param remove_factors (bool): If any eigenvalue, resulting from performing PCA on your data set, is too small for relative
#' importance, it can be removed automatically if this is TRUE. If FALSE, the same situation will produce an error.
#' @param factors_to_remove (int): If remove_factors is TRUE, you can either a) set this to 0 to have the script iteratively
#' remove PCA factors until the relative importance calculation works (recommended if you do not know how many PCA factors to
#' remove, but takes longer), or b) set this to any positive integer smaller than the number of factors. In condition b, the
#' script will go ahead and remove the X smallest factors (X being the number this argument is set to).
#' @param max_factors_to_remove (int): If remove_factors is TRUE and factors_to_remove is 0, then this will determine how many
#' factors the script will delete before "giving up". This is to prevent a possible very long process. This can be set to 0
#' to iterate through all columns (not recommended).
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

RelimpPCR = function(Y,X,target_r2,r2_type="test",validation_split=0.8,relimp_algorithm="last",
                     max_predictors=0,remove_factors=T,factors_to_remove=0,max_factors_to_remove=15,
                     normalize_data=T,plot_this=T,verbose=F,multicore=T){
  
  pr = function(prompt,verbose){
    if(verbose){
      print(paste0(Sys.time()," | ",prompt))
    }
  }

  if(normalize_data == F){
    warning("WARN: Using non-normalized data in PCA can cause sub-optimal results.")
  } else{
    for(z in 1:dim(X)[2]){
      X[,z] = scale(X[,z])
    }
    Y = scale(Y)
  }
  

  pr("Running PCA",verbose)
  
  #PCA
  pca = prcomp(X)
  pca_factors = pca$x
  pca_loadings = pca$rotation
  
  if(validation_split == 1){
    trainX = X; testX = X
    trainY = Y; testY = Y
    trainX_PCA = pca; testX_PCA = pca
  } else if (validation_split <= 0) {
    stop("Validation split cannot be 0 or negative.")
  } else if (validation_split > 1) {
    stop("Validation split cannot be greater than 1.")
  } else {
    pr("Performing Train/Test Split",verbose)
    
    ix = sample(x = 1:dim(X)[1],size = round(dim(X)[1] * validation_split,0))
    trainX = X[ix,]; testX = X[-ix,]
    trainY = Y[ix]; testY = Y[-ix]
    trainX_PCA = pca_factors[ix,]; testX_PCA = pca_factors[-ix,]
  }

    pr(paste0("Ranking predictors against Y using calc.relimp ",relimp_algorithm),verbose)
  
  #Ranking Features
  fit = lm(Y~.,data = data.frame(Y= unlist(trainY),trainX))
  relimp_factors = relaimpo::calc.relimp(fit,type="last")
  ranked_factors = relimp_factors@last.rank
  
  trainX_ordered = trainX[,order(ranked_factors)]
  testX_ordered = testX[,order(ranked_factors)]
  

  pr("Ranking PCA factors against Y using calc.relimp",verbose)
  
  if(max_factors_to_remove == 0){
    max_factors_to_remove = ncol(trainX_PCA)
  }
  
  #PCA Ranking
  if(remove_factors == T){
    if(factors_to_remove == 0){
      for(x in 0:max_factors_to_remove){
        pca_factor_subset = trainX_PCA[,1:(ncol(trainX_PCA) - x)]
        pca_fit = lm(Y~.,data = data.frame(Y = unlist(trainY),pca_factor_subset))
        try({
          pca_relimp_factors = relaimpo::calc.relimp(pca_fit,type="last")
          pr(paste0("PCA factor relative importance calculation successful; Removed ",x," PCA factor(s)"),TRUE)
          break
        })
        
        pr(paste0("ERROR in calculating relative importance of PCA factors; Removing last ",x," PCA factor(s)"),TRUE)
        if(x == max_factors_to_remove){
          stop("Could not create non-singular matrix. Try increasing max_factors_to_remove.")
        }
      }
    } else {
      pca_factor_subset = trainX_PCA[,1:(ncol(trainX_PCA) - factors_to_remove)]
      pca_fit = lm(Y~.,data = data.frame(Y = unlist(trainY),pca_factor_subset))
      pca_relimp_factors = relaimpo::calc.relimp(pca_fit,type="last")
      pr(paste0("Removed ",factors_to_remove," PCA factor(s)."))
    }
  } else {
    pca_factor_subset = trainX_PCA
    pca_fit = lm(Y~.,data = data.frame(Y = unlist(trainY),pca_factor_subset))
    pca_relimp_factors = relaimpo::calc.relimp(pca_fit,type="last")
  }
  
  pca_ranked_factors = pca_relimp_factors@last.rank
  trainX_PCA_ordered = trainX_PCA[,order(pca_ranked_factors)]
  testX_PCA_ordered = testX_PCA[,order(pca_ranked_factors)]

  if(max_predictors > dim(pca_factor_subset)[2]){
    stop("ERROR: You cannot have 'max_predictors' be greater than the total number of remaining PCA factors.")
  }
    
  if(max_predictors <= 0){
    predictors_range = 1:dim(pca_factor_subset)[2]
  } else {
    predictors_range = 1:max_predictors
  }
  
  pr("Iteratively adding predictors according to order/ranking for...",verbose)

  get_r2s = function(z,trainX,trainY,testX,testY){
    if(z == 1){
      trainX_df = trainX[,1:z]
      testX_df = testX[,1:z]
      colnames(trainX_df) = "X1"
      colnames(testX_df) = "X1"
    } else {
      trainX_df = trainX[,1:z]
      testX_df = testX[,1:z]
    }
    this_fit = caret::train(x = trainX_df,y = trainY,method="lm")
    
    train_r2 = cor(predict(this_fit,trainX_df),trainY)^2
    test_r2 = cor(predict(this_fit,testX_df),testY)^2
    
    return(c(train_r2,test_r2))
  }
  
  get_best_model = function(trainX,trainY,train_r2,test_r2,r2_type){
    if(r2_type == "train"){
      best_r2 = which.max(train_r2)
    } else {
      best_r2 = which.max(test_r2)
    }

    return(lm(Y~.,data = data.frame(Y=trainY,trainX[,1:best_r2])))
  }
  
  if(multicore==T){
    pr("Original Features",verbose)
    original_r2 = parallel::mclapply(X = predictors_range, FUN = get_r2s,trainX=trainX,trainY=trainY,testX=testX,testY=testY)
    pr("Ordered Features",verbose)
    relimp_r2 = parallel::mclapply(X = predictors_range, FUN = get_r2s,trainX=trainX_ordered,trainY=trainY,testX=testX_ordered,testY=testY)
    pr("PCA Factors",verbose)
    pca_r2 = parallel::mclapply(X = predictors_range, FUN = get_r2s,trainX=trainX_PCA,trainY=trainY,testX=testX_PCA,testY=testY)
    pr("Ordered PCA Factors",verbose)
    pca_relimp_r2 = parallel::mclapply(X = predictors_range, FUN = getr2s,trainX = trainX_PCA_ordered,trainY=trainY,testX=testX_PCA_ordered,testY=testY)
  } else {
    pr("Original Features",verbose)
    original_r2 = lapply(X = predictors_range, FUN = get_r2s,trainX=trainX,trainY=trainY,testX=testX,testY=testY)
    pr("Ordered Features",verbose)
    relimp_r2 = lapply(X = predictors_range, FUN = get_r2s,trainX=trainX_ordered,trainY=trainY,testX=testX_ordered,testY=testY)
    pr("PCA Factors",verbose)
    pca_r2 = lapply(X = predictors_range, FUN = get_r2s,trainX=trainX_PCA,trainY=trainY,testX=testX_PCA,testY=testY)
    pr("Ordered PCA Factors",verbose)
    pca_relimp_r2 = lapply(X = predictors_range, FUN = getr2s,trainX = trainX_PCA_ordered,trainY=trainY,testX=testX_PCA_ordered,testY=testY)
  }
    
  r2_values = list("original_r2"=original_r2,"relimp_r2"=relimp_r2,"pca_r2"=pca_r2,"pca_relimp_r2"=pca_relimp_r2)
  r2_values_out = list()
  
  for(r2 in names(r2_values)){
    this_r2_train = c()
    this_r2_test = c()
    
    for(x in r2_values[[r2]]){
      this_r2_train[length(this_r2_train)+1] = x[[1]]
      this_r2_test[length(this_r2_test)+1] = x[[2]]
    }
    
    r2_values_out[[paste0(r2,"_train")]] = this_r2_train
    r2_values_out[[paste0(r2,"_test")]] = this_r2_test
  }
  
  pr("Determining optimal model",verbose)
  best_model = get_best_model(trainX = trainX_PCA_ordered, trainY = trainY, train_r2 = r2_values_out[["pca_relimp_r2_train"]], test_r2 = r2_values_out[["pca_relimp_r2_test"]], r2_type = r2_type)
  
  if(plot_this==T){
    par(mfrow=c(1,2))
    plot(predictions_range,cbind(r2_values_out[["original_r2_train"]],r2_values_out[["relimp_r2_train"]],r2_values_out[["pca_r2_train"]],r2_values_out[["pca_relimp_r2_train"]]),
         type="l",lty=1,lwd=2,col=c("black","red","green","blue"),main="Improvement of Fit with Number of Predictors (Training)", xlab = "Number of Predictors",
         ylab = "Determination Coefficient")
    
    plot(predictions_range,cbind(r2_values_out[["original_r2_test"]],r2_values_out[["relimp_r2_test"]],r2_values_out[["pca_r2_test"]],r2_values_out[["pca_relimp_r2_test"]]),
         type="l",lty=1,lwd=2,col=c("black","red","green","blue"),main="Improvement of Fit with Number of Predictors (Testing)", xlab = "Number of Predictors",
         ylab = "Determination Coefficient")
    legend("bottomright",legend=c("Original","PCA","Relimp","PCA w/ Relimp"),lty=1,lwd=2,col=c("black","red","green","blue"))
  }
    
  out = list("values_train "= trainX, "values_test" = testX,"pca_factors_train" = trainX_PCA,"pca_factors_test" = testX_PCA, "pca_loadings" = pca_loadings,
             "pca_ordered_factors_train" = trainX_PCA_ordered, "pca_ordered_factors_test" = testX_PCA_ordered,
             "ordered_predictors_train" = trainX_ordered, "ordered_predictors_test" = testX_ordered,
             "original_r2_train" = r2_values_out[["original_r2_train"]], "original_r2_test" = r2_values_out[["original_r2_test"]],
             "pca_r2_train" = r2_values_out[["pca_r2_train"]], "pca_t2_test" = r2_values_out[["pca_r2_test"]],
             "relimp_pca_r2_train" = r2_values_out[["pca_relimp_r2_train"]], "relimp_pca_r2_test" = r2_values_out[["pca_relimp_r2_test"]],
             "relimp_r2_train" = r2_values_out[["relimp_r2_train"]], "relimp_r2_test" = r2_values_out[["relimp_r2_test"]],"best_model"=best_model)

    pr("Process complete",verbose)
  return(out)
}

