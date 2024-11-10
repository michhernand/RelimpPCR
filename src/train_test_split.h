#ifndef TRAIN_TEST_SPLIT_H
#define TRAIN_TEST_SPLIT_H

#include <string>
#include <armadillo>
#include <unordered_map>
#include "split_data.h"


SplitData train_test_split(const arma::dmat& x, const arma::dvec& y, double train_size);



#endif
