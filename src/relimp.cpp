#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
#include <unordered_map>

class Model {
  public:
    arma::dvec coefficients;
    arma::dvec fitted_values;
    arma::dvec residuals;
    double r_squared;

    Model(
        const arma::dvec& coefficients,
        const arma::dvec& fitted_values,
        const arma::dvec& residuals,
        const double r_squared
    ) : coefficients(coefficients), fitted_values(fitted_values), residuals(residuals), r_squared(r_squared) {};
};

Model lm(
    arma::dmat x,
    arma::dvec y
) {
     arma::vec beta = arma::solve(x, y);
     arma::vec y_pred = x * beta;
     arma::vec residuals = y - y_pred;

     double ss_res = arma::accu(arma::square(residuals));
     double ss_tot = arma::accu(arma::square(y - arma::mean(y)));
     double r_squared = 1 - (ss_res / ss_tot);

     return Model(
         beta, y_pred, residuals, r_squared
     );
}