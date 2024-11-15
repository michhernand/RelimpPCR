#define ARMA_64BIT_WORD
#include <unordered_map>
#include <stdexcept>
#include <armadillo>
#include "model.h"
#include "matutils.h"
#include "column_contribution.h"

class RelimpAlgorithm {
  public:
      virtual arma::uvec sort(arma::dmat x, arma::dvec y) = 0;
      virtual arma::uword n_iter(arma::dmat x) = 0;
};

class LastRelimpAlgorithm : public RelimpAlgorithm {
  public:
      arma::dmat sort(arma::dmat x, arma::dvec y) override {
          double r_squared_arr[x.n_cols];
          for (arma::uword i = 0; i < x.n_cols; ++i) {
            r_squared_arr[i] = lm(remove_col(x, i), y).r_squared;
          }

          return argsort_matrix(x, r_squared_arr);
      }

      /**
	* @brief Gathers r-squared values evaluating the importance of a single value of x.
	* @param x The x matrix (independent variables) of the model.
	* @param y The y vector (dependent variable) of the model.
	* @param column_index The column being evaluated.
	* @return A ColumnContribution object tracking the columns importance.
	*/
      ColumnContribution evaluate_column(
          arma::dmat x,
          arma::dvec y,
          arma::uword column_index
      ) override {
        auto permutations = last_permutations(x.n_cols, column_index);
        auto cc = ColumnContribution(column_index, permutations.size());

        for (arma::uword i = 0; i < permutations.size(); ++i) {
          perm_x_ix = permutations[i];
          perm_x = x.cols(perm_x_ix);

          dual_lm_cc(
		x.col(column_index),
                perm_x,
                y, cc
          );
        }

        return cc
      }


};

arma::dmat reordered_mat(mat.n_rows, mat.n_cols);



void Relimp(
    arma::dmat x,
    arma::dvec y,
    RelimpAlgorithm algo
) {
	std::vector<ColumnContribution> contributions;
        contributions.resize(x.n_cols, ColumnContribution(0, 0));
        for (arma::uword i = 0; i < x.n_cols; ++i) {
        	cc = ColumnContribution(i, algo.n_iter
        	contributions[i] = ColumnContribution(i, algo.n_iter(x));
        }
}