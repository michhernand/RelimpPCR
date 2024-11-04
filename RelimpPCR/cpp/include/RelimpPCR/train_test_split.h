#ifndef TRAIN_TEST_SPLIT_H
#define TRAIN_TEST_SPLIT_H

#include <armadillo>
#include <utility>
#include <unordered_map>
#include <string>
#include "RelimpPCR/split_data.h"


auto train_test_split(
    const arma::dmat& x, 
    const arma::dvec& y, 
    double train_size
) -> SplitData;

#endif
