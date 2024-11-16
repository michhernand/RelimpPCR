#ifndef MATUTILS_H
#define MATUTILS_H
#include <armadillo>

arma::dmat remove_cols(arma::dmat mat, arma::uword col);
std::vector<arma::uvec> last_permutations(arma::uword n_columns, arma::uword skip_index);

#endif //MATUTILS_H
