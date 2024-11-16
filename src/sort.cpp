#include <algorithm>
#include <armadillo>

/**
* @brief Produces the indices of the sorted vec.
* @param arr The arma::vec to sort.
* @param desc Sort in descending order.
* @return A arma::uvec containing the sorted indices.
*/
arma::uvec argsort_array(const arma::dvec& arr, bool desc) {
    arma::uvec indices = arma::regspace<arma::uvec>(0, arr.n_elem - 1);
    std::sort(indices.begin(), indices.end(), [&arr, desc](arma::uword a, arma::uword b) {
      if (desc) {
        return arr[a] > arr[b];
      } 
        return arr[a] < arr[b];
    });
    return indices;
}

