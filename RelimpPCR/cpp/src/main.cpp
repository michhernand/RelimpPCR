#include <iostream>
#include <armadillo>
#include "RelimpPCR/train_test_split.h"
#include "RelimpPCR/normalize.h"

int main() {
    const double TRAIN_SIZE = 0.7;
    const arma::dmat& x = arma::zeros<arma::dmat>(5, 5);
    const arma::dvec& y = arma::zeros<arma::dvec>(5);

    auto split_data = train_test_split(x, y, TRAIN_SIZE);
    auto normalized_data = normalize(split_data);

    auto x_ = split_data.x;
    auto y_ = split_data.y;

    auto x_train = x_["train"];
    auto y_train = y_["train"];

    std::cout << x_train.n_rows << std::endl;
    
    return 0;
}
