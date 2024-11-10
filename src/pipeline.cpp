#define ARMA_64BIT_WORD
#include "train_test_split.h"
#include "split_data.h"
#include "normalize.h"
#include <armadillo>
#include <unordered_map>
#include <tuple>

int RelimpPCR(
    arma::dmat x,
    arma::dvec y,
    double train_size
) {
    SplitData split_data = train_test_split(x, y, train_size);
    SplitData normalized_data = normalize(split_data);

    // arma::mat coeff;
    // arma::mat score;
    // arma::vec latent;
    // arma::vec tsquared;

    // bool ok = arma::princomp(
    //     coeff, 
    //     score, 
    //     latent, 
    //     tsquared, 
    //     normalized_data.x["train"]
    // );

    // if (!ok) {
    //     throw std::runtime_error("pca decomposition failed");
    // }
    //
    // return std::make_tuple(
    //     coeff,
    //     score,
    //     latent,
    //     tsquared
    // );
    return 0;
}
