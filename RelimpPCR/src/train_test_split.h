#ifndef TRAIN_TEST_SPLIT_H
#define TRAIN_TEST_SPLIT_H

#include <armadillo>
#include <utility>
#include <unordered_map>
#include <string>

class SplitData {
public:
    std::unordered_map<std::string, arma::dmat> x;
    std::unordered_map<std::string, arma::dvec> y;

    SplitData(
        const std::unordered_map<std::string, arma::dmat>& x_data,
        const std::unordered_map<std::string, arma::dvec>& y_data
    );
};


SplitData train_test_split(const arma::dmat& x, const arma::dvec& y, double train_size);



#endif
