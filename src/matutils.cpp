#include <armadillo>
#include <vector>
#include <utility>

arma::dmat remove_col(arma::dmat mat, arma::uword col) {
    arma::dmat mat_excluded;

    if (col > 0) {
        mat_excluded = mat.cols(0, col - 1);  // Columns before the excluded column
    }
    if (col < mat.n_cols - 1) {
        mat_excluded = arma::join_rows(mat_excluded, mat.cols(col + 1, mat.n_cols - 1));
    }
    return mat_excluded;
}

std::vector<arma::uvec> last_permutations(arma::uword n_columns, arma::uword skip_index) {
    std::vector<arma::uvec> permutations;

    for (arma::uword i = 1; i <= n_columns; ++i) {
        arma::uvec indices;

        for (arma::uword j = 0; j < i; ++j) {
            if (j != skip_index) {
                indices.insert_rows(indices.n_rows, arma::uvec{ j });
            }
        }
        permutations.push_back(indices);
    }

    return permutations;
}
