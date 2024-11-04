#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
#include <utility>
#include <unordered_map>
#include "train_test_split.h"
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
Rcpp::List train_test_split_r(Rcpp::NumericMatrix x, Rcpp::NumericVector y, double train_size) {
    arma::dmat x_arma = Rcpp::as<arma::dmat>(x);
    arma::dvec y_arma = Rcpp::as<arma::dvec>(y);

    SplitData result = train_test_split(x_arma, y_arma, train_size);

    return Rcpp::List::create(
        Rcpp::Named("x_train") = result.x["train"],
        Rcpp::Named("x_test") = result.x["test"],
        Rcpp::Named("y_train") = result.y["train"],
        Rcpp::Named("y_test") = result.y["test"]
    );
}
