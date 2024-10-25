#define ARMA_64BIT_WORD
#include "Rcpp.h"
#include "train_test_split.h"
#include "normalize.h"
#include <unordered_map>

int RelimpPCR(
    arma::dmat x,
    arma::dvec y,
    double train_size
) {
    std::pair<
        std::unordered_map<std::string, arma::dmat>,
        std::unordered_map<std::string, arma::dvec>
    >  split_data = train_test_split(x, y, train_size);

    std::unordered_map<std::string, arma::dmat> x_split = split_data.first;
    std::unordered_map<std::string, arma::dvec> y_split = split_data.second;

    auto x_train = x_split["train"];
    auto x_test = x_split["test"];
    auto y_train = y_split["train"];
    auto y_test = y_split["test"];

    auto x_train_norm = normalize_df(x_train);
    auto y_train_norm = normalize_vector(y_train);

    return 0;
}
