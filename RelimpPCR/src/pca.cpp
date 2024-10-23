#include <armadillo>

arma::dmat pca(arma::dmat x) {
    arma::mat coeff;
    arma::mat score;
    arma::vec latent;
    arma::vec tsquared;

    arma::princomp(coeff, score, latent, tsquared, x);

    return score;
}
