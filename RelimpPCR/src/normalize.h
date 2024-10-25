#ifndef NORMALIZE_H
#define NORMALIZE_H
#include <armadillo>

std::tuple<arma::dvec, double, double> normalize_vector(arma::dvec);

arma::dmat normalize_df(arma::dmat);

#endif
