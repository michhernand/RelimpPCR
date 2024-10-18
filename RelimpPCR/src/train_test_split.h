#ifndef TRAIN_TEST_SPLIT_H
#define TRAIN_TEST_SPLIT_H

#include <Rcpp.h>
#include <utility>
#include <unordered_map>

std::pair<
    std::unordered_map<std::string, Rcpp::DataFrame>,
    std::unordered_map<std::string, Rcpp::NumericVector>
> train_test_split(Rcpp::DataFrame x, Rcpp::NumericVector y, double train_size);

#endif
