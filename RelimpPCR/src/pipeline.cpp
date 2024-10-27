#define ARMA_64BIT_WORD
#include "Rcpp.h"
#include "train_test_split.h"
#include "normalize.h"
#include <unordered_map>

arma::dmat RelimpPCR(
    arma::dmat x,
    arma::dvec y,
    double train_size
) {
    std::pair<
        std::unordered_map<std::string, arma::dmat>,
        std::unordered_map<std::string, arma::dvec>
    > split_data = train_test_split(x, y, train_size);

    std::pair<
        std::unordered_map<std::string, arma::dmat>,
        std::unordered_map<std::string, arma::dvec>
    > normalized_data = normalize(split_data);

    arma::mat coeff;
    arma::mat score;
    arma::vec latent;
    arma::vec tsquared;

    bool ok = arma::princomp(
        coeff, 
        score, 
        latent, 
        tsquared, 
        normalized_data.first["train"]
    );

    return normalized_data.first["train"];
}
