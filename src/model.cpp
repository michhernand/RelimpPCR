#include <utility>
#include <armadillo>
#include "model.h"
#include "column_contribution.h"

Model basic_lm(
    const arma::dmat& x,
    const arma::dvec& y
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

/**
* @brief Fits two models. One WITH the x column at `toggle_index` included. One WITHOUT the x column at `toggle_index` included.
* @param toggle_index The column index to include/exclude when training models.
* @param x The x matrix (independent variables) of the model.
* @param y The y vector (dependent variable) of the model.
* @return A pair of Model objects. The first model DOES have toggle index. The second model DOES NOT have toggle index.
*/
std::pair<Model, Model> dual_lm(
    const arma::dvec& toggle_vec,
    const arma::dmat& x,
    const arma::dvec& y
) {
    Model mod_without_column = basic_lm(x, y);

    arma::dmat xx = x;
    xx.insert_cols(xx.n_cols, toggle_vec);
    Model mod_with_column = basic_lm(xx, y);
    return std::make_pair(mod_with_column, mod_without_column);
}

/**
* @brief Fits two models using dual_lm. Adds the results to a ColumnContribution object.
* @param toggle_index The column index to include/exclude when training models.
* @param x The x matrix (independent variables) of the model.
* @param y The y vector (dependent variable) of the model.
* @param cc The ColumnContribution object tracking the r-squared contribution of a column of x.
*/
void dual_lm_cc(
    const arma::dvec& toggle_vec,
    const arma::dmat& x,
    const arma::dvec& y,
    ColumnContribution& cc
) {
    std::pair<Model, Model> models = dual_lm(toggle_vec, x, y);
    cc.set_next(models.first.r_squared, models.second.r_squared);
}
