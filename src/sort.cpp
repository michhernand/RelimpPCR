#include <algorithm>
#include <armadillo>

/**
* @brief Produces the indices of the sorted vec.
* @param arr The arma::vec to sort.
* @return A arma::uvec containing the sorted indices.
*/
arma::uvec argsort_array(const arma::dvec& arr) {
    arma::uvec indices = arma::regspace<arma::uvec>(0, arr.n_elem - 1);
    std::sort(indices.begin(), indices.end(), [&arr](arma::uword a, arma::uword b) {
        return arr[a] < arr[b];
    }
    return indices;
}

/**
* @brief Produces a matrix with columns sorted according to indices.
* @param mat The matrix to sort the columns of.
* @param indices The order by which to sort the columns.
* @return A arma::dmat containing sorted columns.
*/
arma::dmat argsort_matrix(const arma::dmat& mat, const arma::uvec& indices) {
    if (keys.n_elem != mat.n_cols) {
        throw std::invalid_argument("size of keys vector must match the number of columns in the matrix");
    }

    arma::dmat reordered_mat(mat.n_rows, mat.n_cols);
    for (arma::uword i = 0; i < indices.n_elem; ++i) {
        reordered_mat.col(i) = mat.col(indices[i]);
    }
    return reordered_mat;
}

/**
* @brief Produces a matrix with columns sorted by their contribution to rsquared.
* @param mat The matrix to sort the columns of.
* @param rsquared A vector of rsquared measurements (length must match number of columns in mat).
* @return A arma::dmat containing sorted columns.
*/
arma::dmat sort_matrix_by_rsquared(const arma::dmat& mat, const arma::dvec& rsquared) {
    if (keys.n_elem != mat.n_cols) {
        throw std::invalid_argument("size of rsqaured vector must match the number of columns in the matrix");
    }

    arma::dvec sorted_rsquared = sort_array(rsquared);
    return argsort_matrix(mat, sorted_rsquared);
}