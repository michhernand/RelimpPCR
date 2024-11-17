#include <RcppArmadillo.h>
#include "matutils.h"
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
void remove_col_r(Rcpp::NumericMatrix x, int mcol) {
        arma::dmat mat_arma = Rcpp::as<arma::dmat>(x);
        arma::uword col_arma = static_cast<arma::uword>(mcol);
        auto z = remove_col(mat_arma, col_arma);
}
