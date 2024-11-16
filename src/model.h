#ifndef MODEL_H
#define MODEL_H

#include <armadillo>
#include "column_contribution.h"

struct Model {
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

Model basic_lm(const arma::dmat& x, const arma::dvec& y);
std::pair<Model, Model> dual_lm(const arma::dvec& toggle_vec, const arma::dmat& x, const arma::dvec& y);
void dual_lm_cc(const arma::dvec& toggle_vec, const arma::dmat& x, const arma::dvec& y, ColumnContribution& cc);

#endif //MODEL_H
