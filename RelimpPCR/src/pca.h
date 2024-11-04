#ifndef PCA_H
#define PCA_H
#define AMRA_64BIT_WORD
#include <armadillo>
#include <tuple>

std::tuple<
    arma::dmat,
    arma::dmat,
    arma::dvec,
    arma::dvec
> backwards_step_princomp(
    arma::dmat x
);

#endif
