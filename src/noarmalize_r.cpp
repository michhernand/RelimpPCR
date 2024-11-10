#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
#include <tuple>
#include "normalize.h"
#include "train_test_split.h"
#include "split_data.h"
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List split_and_normalize_r(Rcpp::NumericMatrix x, Rcpp::NumericVector y, double train_size) {
    arma::dmat x_arma = Rcpp::as<arma::dmat>(x);
    arma::dvec y_arma = Rcpp::as<arma::dvec>(y);

    SplitData split_data = train_test_split(x_arma, y_arma, train_size);

    SplitData normalized_data = normalize(split_data);

    return Rcpp::List::create(
        Rcpp::Named("x_train") = Rcpp::wrap(normalized_data.x["train"]),
        Rcpp::Named("x_test") = Rcpp::wrap(normalized_data.x["test"]),
        Rcpp::Named("y_train") = Rcpp::wrap(normalized_data.y["train"]),
        Rcpp::Named("y_test") = Rcpp::wrap(normalized_data.y["test"])
    );
}
