#ifndef NORMALIZE_H
#define NORMALIZE_H
#define AMRA_64BIT_WORD
#include <armadillo>
#include "split_data.h"

std::tuple<arma::dvec, double, double> normalize_vector(arma::dvec);

arma::dvec normalize_vector_pred(
    arma::dvec,
    double,
    double
);

std::pair<
    arma::dmat,
    std::vector<
        std::pair<
            double,
            double
        >
    >
> normalize_df(arma::dmat);

arma::dmat normalize_df_pred(
    arma::dmat,
    std::vector<
        std::pair<
            double,
            double
        >
    >
);

SplitData normalize(
    SplitData split_data
);

#endif
