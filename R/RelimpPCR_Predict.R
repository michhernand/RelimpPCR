#' Predictor Function for RelimpPCR
#' 
#' This function takes the trained RelimpPCR object and proceeds to perform a prediction from the best model (as defined in the documentation of RelimpPCR()).
#' @param pcr (pcr_object): The trained RelimpPCR object produced by the RelimpPCR() function.
#' @param newdata (data frame): The new X value(s) you wish to draw a prediction from.
#' @return pred (data frame): A data frame containing the preictions.
#' @examples 
#' \donttest{
#' #The below function takes a trained PCR object (produced by RelimpPCR) 
#' #and a dataframe (using the same columns that the PCR object was trained 
#' #with) and produces a prediction.
#' y = mtcars$mpg[1:20]; x = mtcars[1:20,c("cyl","disp")]
#' pcr_object = RelimpPCR(Y = y, X = x,target_r2 = 0.75, multicore = FALSE,
#'                        remove_factors = FALSE, normalize_data = FALSE, plot_this = FALSE)
#' pred = RelimpPCR.predict(pcr_object, data.frame(mtcars$cyl, mtcars$disp))
#' }
#' @export
RelimpPCR.predict <- function(pcr, newdata) {
  single_prediction <- FALSE
  if (!is(newdata, "data.frame")) {
    single_prediction <- TRUE
    newdata <- data.frame(t(unlist(newdata)))
  }
  colnames(newdata) <- pcr$initial_colnames

  if (is.null(pcr$scaling_factors) == FALSE) {
    for (j in 1:ncol(newdata)) {
      newdata[, j] <- (
        newdata[, j] - pcr$scaling_factors$X_means[j]
      ) / pcr$scaling_factors$X_st_devs[j]
    }
  }

  pca <- predict(pcr$pca_object, newdata)
  ordered_pca <- as.data.frame(pca[, order(pcr$pca_factors_rank)])

  if (single_prediction == TRUE) {
    pca_colnames <- row.names(ordered_pca)
    ordered_pca <- data.frame(t(unlist(ordered_pca)))
    colnames(ordered_pca) <- pca_colnames
  }
  ordered_pca <- ordered_pca[, 1:pcr$num_factors]

  colnames(ordered_pca) <- names(
    pcr$pca_factors_rank)[order(pcr$pca_factors_rank)]

  pred <- predict(pcr$best_model, ordered_pca)
  return(pred)
}