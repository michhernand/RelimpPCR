#ifndef PIPELINE_H
#define PIPELINE_H

#include <armadillo>
#include <utility>
#include <unordered_map>

arma::dmat RelimpPCR(
    arma::dmat x,
    arma::dvec y,
    double train_size
);

#endif
