#include <RcppArmadillo.h>
#include "train_test_split.h"
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List train_test_split_r(Rcpp::DataFrame x, Rcpp::NumericVector y, double train_size = 0.7) {
    // Convert DataFrame to arma::dmat
    arma::dmat x_arma = Rcpp::as<arma::dmat>(Rcpp::as<Rcpp::NumericMatrix>(x));

    // Convert NumericVector to arma::dvec
    arma::dvec y_arma = Rcpp::as<arma::dvec>(y);

    // Call the core function
    auto result = train_test_split(x_arma, y_arma, train_size);

    // Convert results back to R types
    Rcpp::NumericMatrix x_train = Rcpp::wrap(result.first["train"]);
    Rcpp::NumericMatrix x_test = Rcpp::wrap(result.first["test"]);
    Rcpp::NumericVector y_train = Rcpp::wrap(result.second["train"]);
    Rcpp::NumericVector y_test = Rcpp::wrap(result.second["test"]);

    // Create DataFrames from matrices (if needed)
    Rcpp::DataFrame x_train_df = Rcpp::as<Rcpp::DataFrame>(x_train);
    Rcpp::DataFrame x_test_df = Rcpp::as<Rcpp::DataFrame>(x_test);

    // Copy column names from input DataFrame
    Rcpp::CharacterVector col_names = x.names();
    x_train_df.names() = col_names;
    x_test_df.names() = col_names;

    return Rcpp::List::create(
        Rcpp::Named("x_train") = x_train_df,
        Rcpp::Named("x_test") = x_test_df,
        Rcpp::Named("y_train") = y_train,
        Rcpp::Named("y_test") = y_test
    );
}
