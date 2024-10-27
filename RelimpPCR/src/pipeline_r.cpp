#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
#include <tuple>
#include "pipeline.h"
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::NumericMatrix RelimpPCR(
    Rcpp::NumericMatrix x,
    Rcpp::NumericVector y,
    double train_size
) {
    arma::dmat x_arma = Rcpp::as<arma::dmat>(x);
    arma::dvec y_arma = Rcpp::as<arma::dvec>(y);

    return Rcpp::wrap(RelimpPCR(x_arma, y_arma, train_size));
}
