#ifndef TRAIN_TEST_SPLIT_H
#define TRAIN_TEST_SPLIT_H

#include <armadillo>
#include <utility>
#include <unordered_map>

std::pair<
    std::unordered_map<std::string, arma::dmat>,
    std::unordered_map<std::string, arma::dvec>
> train_test_split(const arma::dmat& x, const arma::dvec& y, double train_size);

#endif
