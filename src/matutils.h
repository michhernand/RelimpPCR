#ifndef MATUTILS_H
#define MATUTILS_H
#include <armadillo>

inline arma::dmat remove_col(arma::dmat source_matrix, arma::uword column_index) {
        arma::dmat mat_excluded;

        if (column_index > 0) {
                mat_excluded = source_matrix.cols(0, column_index - 1);  
        }
        if (column_index < source_matrix.n_cols - 1) {
                mat_excluded = arma::join_cols(
                                mat_excluded, 
                                source_matrix.cols(
                                        column_index + 1, 
                                        source_matrix.n_cols - 1
                                        )
                                );
        }
        return mat_excluded;
}


std::vector<arma::uvec> last_permutations(arma::uword n_columns, arma::uword skip_index);

#endif //MATUTILS_H
