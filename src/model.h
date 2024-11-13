//
// Created by Michael Hernandez on 11/12/24.
//

#ifndef MODEL_H
#define MODEL_H

#include <armadillo>

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

#endif //MODEL_H
