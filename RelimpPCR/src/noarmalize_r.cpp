#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
#include <tuple>
#include "normalize.h"
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::NumericMatrix normalize_df_r(Rcpp::NumericMatrix x) {
    arma::dmat x_arma = Rcpp::as<arma::dmat>(x);
    return Rcpp::wrap(normalize_df(x_arma));
}
