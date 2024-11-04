#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
#include <tuple>
#include "pca.h"
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List backwards_step_princomp_r(
    Rcpp::NumericMatrix x
) {
    std::tuple<
        arma::mat,
        arma::mat,
        arma::vec,
        arma::vec
    > out = backwards_step_princomp(Rcpp::as<arma::dmat>(x));
    std::cout << "Received out!" << std::endl;
    return Rcpp::List::create(
        Rcpp::Named("coeff") = Rcpp::wrap(std::get<0>(out)),
        Rcpp::Named("score") = Rcpp::wrap(std::get<1>(out)),
        Rcpp::Named("latent") = Rcpp::wrap(std::get<2>(out)),
        Rcpp::Named("tsquared") = Rcpp::wrap(std::get<3>(out))
    );
}
