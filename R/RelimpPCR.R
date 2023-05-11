#' A Relative Importance PCA Regression Function
#' 
#' This function performs a relative importance PCA regression. It performs PCA and then applys a relative
#' importnace measure on each additional factor. The output shows optimal PCA factor selection for a given
#' regression.
#' @param Y (list/vector): This a list/vector of Y values for the regression.
#' @param X (data frame): This is the input data for the regression.
#' @param target_r2 (float 0-1): The algorithm will attempt to return to you the simplest model (i.e. with fewest predictors) that satisfies
#' your target_r2 value; If no model satisfies this condition, then the full model (with all predictors) will be returned.
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
#' @param plot_this (bool): Whether or not to plot the r-squared values. Default is TRUE.
#' @param verbose (bool): Whether or not to include some additional narration around the status of the process.
#' Default is FALSE.
#' @param multicore (bool): Whether or not to use mclapply instead of sapply. Default is TRUE.
#' @param cores (int): The number of cores to distribute work across for multicore operations.
#' @param random_seed (int): Random seed (if you wish to use one). NA indicates no random seed.
#' @return out (list): A list containing all of the below components...
#' @return $pca_loadings: The PCA loadings.
#' @return $pca_object: The trained PCA object.
#' @return $pca_factors_rank: The numerical ranking of the PCA factors.
#' @return $original_r2_train: The r-squared values when iteratively adding unordered training predictors.
#' @return $pca_r2_train: The r-squared values when iteratively adding unordered training PCA factors.
#' @return $relimp_pca_r2_train: The r-squared values when iteratively adding ordered training PCA factors (ordered by relative importance of the training data set).
#' @return $best_model: The model with the fewest predictors that has r-squared equal to or above the "target_r2" argument.
#' @return $num_factors: The number of PCA factors used in the best model.
#' @return $scaling_factors: The mean and standard deviations used to scale the X columns and Y column.
#' @return $relimp_r2_train: ONLY RETURNED IF relative importance for ordered predictors is successful. This contains the r-squared values when iteratively adding ordered predictors (ordered by relative importance of the training data set).
#' @return $ranked_features: ONLY RETURNED IF relative importance for ordered predictors is successful. This contains the numerical ranking of predictors.
#' @return $original_r2_test: ONLY RETURNED IF validation_split argument is not equal to 1. This contains the r-squared values when iteratively adding unordered testing predictors.
#' @return $pca_r2_test: ONLY RETURNED IF validation_split argument is not equal to 1: This contains the r-squared values when iteratively adding unordered testing PCA factors.
#' @return $relimp_pca_r2_test: ONLY RETURNED IF validation_split argument is not equal to 1. This contains the r-squared values when iteratively adding ordered testing PCA factors (ordered by relative importance of the training data set).
#' @return $relimp_r2_test: ONLY RETURNED IF validation_split argument is not equal to 1 AND relative importance for ordered predictors is successful. This contains the r-squared values when iteratively adding ordered testing predictors (ordered by relative importance of the training data set).
#' @examples 
#' \donttest{
#' #Below performs single core relative importance principal 
#' #components regression of mpg against cyl, disp, and hp (all from the mtcars 
#' #sample data set), optimizing for a r-squared value of 0.75.
#' y = mtcars$mpg[1:20]; x = mtcars[1:20,c("cyl","disp")]
#' pcr_object = RelimpPCR(Y = y, X = x,target_r2 = 0.75, multicore = FALSE,
#' remove_factors = FALSE, normalize_data = FALSE, plot_this = FALSE)
#' }
#' @export
RelimpPCR <- function(
  Y,X,target_r2,validation_split=1,relimp_algorithm="last",
  max_predictors=0,remove_factors=T,factors_to_remove=0,
  max_factors_to_remove=15,normalize_data=T,plot_this=T,
  verbose=F,multicore=T,cores=2,random_seed=NA){
  suppressWarnings(RNGversion("3.5.0"))
  if (is.na(random_seed) == FALSE) {
    set.seed(random_seed)
  }

  initial_colnames <- colnames(X)

  dfs <- train_test_split(X, Y, validation_split)

  X <- NULL
  Y <- NULL
  gc()

  if (normalize_data == FALSE) {
    warning(
      "WARN: Using non-normalized data in PCA can cause 
      sub-optimal results")
  } else {
    dfs <- normalize(
      dfs$train_x,
      dfs$train_y,
      dfs$test_x,
      dfs$test_y
    )
  }

  #PCA
  pca <- prcomp(dfs$train_x)
  train_x_pca <- pca$x
  test_x_pca <- predict(pca,dfs$test_x)
  pca_loadings <- pca$rotation
  
  # pr(paste0("Ranking predictors against Y using calc.relimp ",relimp_algorithm),verbose)
  raw_ranked_features <- rank_features_by_relimp(
    dfs,
    relimp_algorithm
  )
  

  # pr("Ranking PCA factors against Y using calc.relimp",verbose)

  if (max_factors_to_remove == 0) {
    max_factors_to_remove <- ncol(train_x_pca)
  }

  #PCA Ranking
  if (remove_factors == TRUE) {
    if (factors_to_remove == 0) {
      pca_ranked <- iteratively_remove_features(
        train_x_pca, dfs,
        max_factors_to_remove, relimp_algorithm)
    } else {
      pca_ranked <- staticly_remove_features(
        train_x_pca, dfs,
        factors_to_remove, relimp_algorithm)
      # pr(paste0("Removed ",factors_to_remove," PCA factor(s)."),verbose)
    }
  } else {
    pca_ranked <- staticly_remove_features(
      train_x_pca, dfs,
      0, relimp_algorithm)
  }

  if (max_predictors > dim(pca_ranked$train_x_ordered)[2]) {
    stop(
      "ERROR: You cannot have 'max_predictors' be greater than the 
      total number of remaining PCA factors.")
  }

  if (max_predictors <= 0) {
    predictors_range <- 1:dim(pca_ranked$train_x_ordered)[2]
  } else {
    predictors_range <- 1:max_predictors
  }
  
  # pr("Iteratively adding predictors according to order/ranking for...",verbose)
  
  if (multicore == TRUE) {
    r2s <- get_r2s_batch_mp(
      predictors_range,
      dfs,
      raw_ranked_features,
      pca_ranked,
      train_x_pca,
      test_x_pca,
      cores,
      verbose
    )
  } else {
    r2s <- get_r2s_batch(
      predictors_range,
      dfs,
      raw_ranked_features,
      pca_ranked,
      train_x_pca,
      test_x_pca,
      verbose
    )
  }

  r2_values_out <- list()

  for (r2 in names(r2s)) {
    this_r2_train <- c()
    this_r2_test <- c()

    for (x in r2s[[r2]]) {
      this_r2_train[length(this_r2_train) + 1] <- x[[1]]
      this_r2_test[length(this_r2_test) + 1] <- x[[2]]
    }

    r2_values_out[[paste0(r2, "_train")]] <- this_r2_train
    r2_values_out[[paste0(r2, "_test")]] <- this_r2_test
  }

  log::log_info("determining optimal model")
  best_model <- get_best_model(
    dfs = list(
      train_x = pca_ranked$train_x_ordered,
      train_y = dfs$train_y
    ),
    train_r2 = r2_values_out[["pca_relimp_r2_train"]],
    test_r2 = r2_values_out[["pca_relimp_r2_test"]]
  )

  if (plot_this == TRUE) {
    plot_results(raw_ranked_features, r2_values_out, validation_split)
    # } else {
    #   print(p1)
    # }
  }
  
  out = list()
  
  out[["pca_loadings"]] = pca_loadings
  out[["pca_object"]] = pca
  out[["pca_factors_rank"]] = pca_ranked$ranked_factors #pca_ranked_factors
  
  out[["original_r2_train"]] = r2_values_out[["original_r2_train"]]
  out[["pca_r2_train"]] = r2_values_out[["pca_r2_train"]]
  out[["relimp_pca_r2_train"]] = r2_values_out[["pca_relimp_r2_train"]]
  
  out[["best_model"]]=best_model
  out[["num_factors"]] = length(best_model$coefficients) - 1
  
  if(normalize_data==T){
    # TODO: Get scaling factors from normalization and include here
    # out[["scaling_factors"]] = list("X_means" = train_means, "X_st_devs" = train_sds, "Y_mean" = Y_mean, "Y_st_dev" = Y_sd)
  }
  
  out[["initial_colnames"]] = initial_colnames
  
  # TODO: include this stuff again
  # if(ranking_successful==T){
  #   out[["relimp_r2_train"]] = r2_values_out[["relimp_r2_train"]]
  #   out[["ranked_features"]] = ranked_factors
  # }
  
  # if(validation_split!=1){
  #   out[["pca_ordered_factors_test"]] = testX_PCA_ordered
  #   out[["original_r2_test"]] = r2_values_out[["original_r2_test"]]
  #   out[["pca_r2_test"]] = r2_values_out[["pca_r2_test"]]
  #   out[["relimp_pca_r2_test"]] = r2_values_out[["pca_relimp_r2_test"]]
    
  #   if(ranking_successful==T){
  #     out[["relimp_r2_test"]] = r2_values_out[["relimp_r2_test"]]
  #   }
  # }
  
  # pr("Process complete",verbose)
  return(out)
}

