#define ARMA_64BIT_WORD
#include <unordered_map>
#include <stdexcept>
#include <armadillo>
#include <model.h>

class RelimpAlgorithm {
  public:
      virtual arma::uvec sort(arma::dmat x, arma::dvec y) = 0;
};

class LastRelimpAlgorithm : public RelimpAlgorithm {
  public:
      arma::dmat sort(arma::dmat x, arma::dvec y) {
          double r_squared_arr[x.n_cols];
          for (arma::uword i = 0; i < x.n_cols; ++i) {
            r_squared_arr[i] = lm(remove_col(x, i), y).r_squared;
          }

          return argsort_matrix(x, r_squared_arr);
      }
};

amra::dvec remove_col(arma::dmat x, arma::uword col) {
    arma::dmat mat_excluded;

    if (col > 0) {
        mat_excluded = mat.cols(0, col - 1);  // Columns before the excluded column
    }
    if (col < mat.n_cols - 1) {
        mat_excluded = arma::join_rows(mat_excluded, mat.cols(col + 1, mat.n_cols - 1));
    }
    return mat_excluded;
}

// TODO: Return sorted_mat and indices.
arma::dmat argsort_matrix(const arma::dmat& mat, const arma::vec& keys) {
    if (keys.n_elem != mat.n_cols) {
        throw std::invalid_argument("Size of keys vector must match the number of columns in the matrix.");
    }

    arma::uvec indices = arma::regspace<arma::uvec>(0, keys.n_elem - 1);  // [0, 1, ..., n_cols-1]
    indices = arma::sort_index(keys);

    arma::dmat sorted_mat(mat.n_rows, mat.n_cols);
    for (arma::uword i = 0; i < indices.n_elem; ++i) {
        sorted_mat.col(i) = mat.col(indices[i]);
    }

    return sorted_mat;
}

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

void Relimp(
    arma::dmat x,
    arma::dvec y,
    std::string algorithm
) {

}