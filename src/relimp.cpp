#define ARMA_64BIT_WORD
#include <armadillo>
#include <utility>
#include <vector>
#include "model.h"
#include "sort.h"
#include "matutils.h"
#include "column_contribution.h"

class RelimpAlgorithm {
  public:
      virtual arma::uword n_iter(arma::dmat x) = 0;
      virtual ColumnContribution evaluate_column(arma::dmat x, arma::dvec y, arma::uword column_index) = 0;
      virtual std::vector<ColumnContribution> evaluate_columns(arma::dmat x, arma::dvec y) = 0;
      virtual std::pair<arma::uvec, arma::dvec> sort_columns(arma::dmat x, arma::dvec y) = 0;
};

class LastRelimpAlgorithm : public RelimpAlgorithm {
  public:
        arma::uword n_iter(arma::dmat x) override {
                return x.n_cols;
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
          auto perm_x_ix = permutations[i];
          auto perm_x = x.cols(perm_x_ix);

          dual_lm_cc(
		x.col(column_index),
                perm_x,
                y, cc
          );
        }
        return cc;
      }

	/**
	* @brief Gathers r-squared values evaluating the importance of all x columns.
	* @param x The x matrix (independent variables) of the model.
	* @param y The y vector (dependent variable) of the model.
	* @return A vector of ColumnContribution objects (one per x column).
	*/
	std::vector<ColumnContribution> evaluate_columns(
	    arma::dmat x,
	    arma::dvec y
	) override {
		std::vector<ColumnContribution> ccs;
		ccs.resize(x.n_cols, ColumnContribution(0, 0));

		for (arma::uword i = 0; i < x.n_cols; ++i) {
			ccs[i] = evaluate_column(x, y, i);
		}
		return ccs;
      }

      	/**
	* @brief Gathers averaged r-squared values evaluating the importance of all x columns.
	* @param x The x matrix (independent variables) of the model.
	* @param y The y vector (dependent variable) of the model.
	* @return A pair containing a) the column indexes sorted most important to least important, and b) the average r-squared lift for those columns.
	*/
      std::pair<arma::uvec, arma::dvec> sort_columns(arma::dmat x, arma::dvec y) override {
        auto result = evaluate_columns(x, y);

        arma::dvec result_avgs(result.size());
        for (arma::uword i = 0; i < result.size(); ++i) {
          result_avgs[i] = result[i].get_lift();
        }

        arma::uvec result_order = argsort_array(result_avgs, true);
        return std::make_pair(result_order, result_avgs(result_order));
      }
};




void Relimp(
    arma::dmat x,
    arma::dvec y,
    LastRelimpAlgorithm algo
) {
	std::vector<ColumnContribution> contributions;
        contributions.resize(x.n_cols, ColumnContribution(0, 0));
        for (arma::uword i = 0; i < x.n_cols; ++i) {
        	auto cc = ColumnContribution(i, algo.n_iter(x));
        	contributions[i] = ColumnContribution(i, algo.n_iter(x));
        }
}
