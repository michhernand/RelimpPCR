#define ARMA_64BIT_WORD
#include <armadillo>
#include <tuple>

std::tuple<
    arma::mat,
    arma::mat,
    arma::vec,
    arma::vec
> backwards_step_princomp(
    std::pair<
        std::unordered_map<std::string, arma::dmat>,
        std::unordered_map<std::string, arma::dvec>
    > normalized_data
) {
    arma::mat coeff;
    arma::mat score;
    arma::vec latent;
    arma::vec tsquared;

    bool ok = false;
    arma::dmat x = normalized_data.first["train"];

    for (arma::uword i = 0; i < x.n_cols; i++) {
        arma::dmat subset = x.head_cols(x.n_cols - i);
        
        ok = arma::princomp(
            coeff, 
            score, 
            latent, 
            tsquared, 
            x
        );

        if (ok) {
            break;
        }
    }

    if (!ok) {
        throw std::runtime_error("backwards step pca decomposition failed");
    }

    return std::make_tuple(
        coeff, 
        score, 
        latent, 
        tsquared
    );
}

