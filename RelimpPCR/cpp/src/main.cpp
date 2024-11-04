#include <armadillo>
#include "RelimpPCR/train_test_split.h"

int main() {
    const double TRAIN_SIZE = 0.7;
    const arma::dmat& x = arma::zeros<arma::dmat>(5, 5);
    const arma::dvec& y = arma::zeros<arma::dvec>(5);
    train_test_split(x, y, TRAIN_SIZE);
    return 0;
}
