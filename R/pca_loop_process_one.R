

#' Removes N Factors and Tries to Compute Relative Importance
#'
#' Given a X matrix, y values, and n factors to remove, this function
#' tries to produce an updated relative importance.
#' @param n (int): The number of factors to remove.
#' @param train_x_pca (data frame): The X matrix.
#' @param train_y (numeric): The y values.
#' @return (list): A list containing a flag indicating if relative
#' importance could be calculated, and the output of the
#' relative importance calculation.
find_factor_subset <- function(
  n,
  train_x_pca,
  train_y
) {
  pca_factor_subset <- train_x_pca[, 1:(ncol(train_x_pca) - n)]
  pca_fit <- lm(
    Y ~ .,
    data = data.frame(Y = unlist(train_y), pca_factor_subset)
  )
  try({
    pca_relimp_factors <- relaimpo::calc.relimp(
      pca_fit,
      type = "last"
    )

    logger::log_info(
      paste(
        "PCA factor relative importance calculation successful; 
        Removed", n, "PCA factor(s)"
      )
    )
    return(
      list(
        ok = TRUE,
        pca_relimp_factors = pca_relimp_factors
      )
    )
  })

  logger::log_warn(
    paste(
      "ERROR in calculating relative importance of PCA factors; 
      Recovering... removing last", n, "PCA factor(s)"
    )
  )

  if (n == ncol(train_x_pca)) {
    logger::log_error(
      "Failed to create non-singular matrix. 
      Try increasing max_factors_to_remove."
    )
    return(
      list(
        ok = FALSE,
        pca_relimp_factors = NULL
      )
    )
  }

  return(
    list(
      ok = TRUE,
      pca_relimp_factors = NULL
    )
  )

}
