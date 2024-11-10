#define ARMA_64BIT_WORD
#include <armadillo>
#include <tuple>

std::tuple<
    arma::mat,
    arma::mat,
    arma::vec,
    arma::vec
> backwards_step_princomp(
    arma::dmat x
) {
    arma::mat coeff;
    arma::mat score;
    arma::vec latent;
    arma::vec tsquared;

    bool ok = false;

    for (arma::uword i = 0; i < x.n_cols; i++) {
        arma::uword subset_ix = x.n_cols - i;
        if (subset_ix <= 1) {
            throw std::runtime_error("backwards step pca decomposition failed");
        }
        arma::dmat subset = x.head_cols(subset_ix);
        
        ok = arma::princomp(
            coeff, 
            score, 
            latent, 
            tsquared, 
            subset
        );

        if (ok) {
            break;
        }
    }

    if (!ok) {
        throw std::runtime_error("backwards step pca decomposition failed");
    }

    std::cout << "Coef Col x Row: " << coeff.n_cols << " x " << coeff.n_rows << std::endl;
    std::cout << "Score Col x Row: " << score.n_cols << " x " << score.n_rows << std::endl;

    return std::make_tuple(
        coeff, 
        score, 
        latent, 
        tsquared
    );
}

