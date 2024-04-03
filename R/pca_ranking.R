
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


#' Removes 0-N Factors and Tries to Compute Relative Importance
#'
#' Given a X matrix, y values, and a maximum number of factors to remove,
#' this function tries to produce a relative importance.
#' @param max_factors_to_remove (int): The maximum number of factors to remove.
#' @param train_x_pca (data frame): The X matrix.
#' @param train_y (numeric): The y values.
#' @return (list): A list containing a flag indicating if relative
#' importance could be calculated, and the output of the
#' relative importance calculation.
find_all_factor_subset <- function(
  max_factors_to_remove,
  train_x_pca,
  train_y
) {
  for (x in 0:max_factors_to_remove) {
    result <- find_factor_subset(x, train_x_pca, train_y)
    if (!result$ok || !is.na(result$pca_relimp_factors)) {
      return(result)
    }
  }
  return(
    list(
      ok = FALSE,
      pca_relimp_factors = NULL
    )
  )
}

#' Computes Relative Importance
#'
#' Given a X matrix and y values, this function produces a relative importance.
#' @param train_x_pca (data frame): The X matrix.
#' @param train_y (numeric): The y values.
#' @return (list): A list containing a flag indicating if relative
#' importance could be calculated, and the output of the
#' relative importance calculation.
get_all_factors <- function(
  train_x_pca,
  train_y
) {
  pca_fit <- lm(
    train_y ~ .,
    data = data.frame(Y = unlist(train_y), train_x_pca)
  )
  pca_relimp_factors <- relaimpo::calc.relimp(pca_fit, type = "last")

  return(
    list(
      ok = TRUE,
      pca_relimp_factors = pca_relimp_factors
    )
  )
}

#' Computes Iterative PCA Ranking
#'
#' Given a X matrix and y values, this function produces a relative importance.
#' @param train_x_pca (data frame): The X matrix.
#' @param train_y (numeric): The y values.
#' @param factors_to_remove (int): Number of factors to remove from data frame.
#' If 0, then no factors will be removed. If -1, then factors will be
#' iteratively removed until relative importance can be calculated.
#' @param max_factors_to_remove (int): If factors_to_remove != 0, then
#' this dictates the maximum number of factors that can be removed.
#' @return (list): A list containing a flag indicating if relative
#' importance could be calculated, and the output of the
#' relative importance calculation.
pca_ranking <- function(
  train_x_pca,
  train_y,
  factors_to_remove,
  max_factors_to_remove
) {
  if (factors_to_remove == -1L) {
    factors <- find_all_factor_subset(
      max_factors_to_remove,
      train_x_pca,
      train_y
    )

    if (!factors$ok) {
      return(factors)
    }

    if (!is.null(factors$pca_relimp_factors)) {
      return(factors)
    }
  } else {
    return(find_factor_subset(factors_to_remove, train_x_pca, train_y))
  }
}
