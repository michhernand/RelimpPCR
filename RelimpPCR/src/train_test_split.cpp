#include <armadillo>
#include <utility>
#include <unordered_map>


/**
* @brief Documents how to perform a train/test split. Provides indices for elements in train and elements in test.
* @param n The size of the collection to be split.
* @param train_size The proprortion of the collection allocated to the training dataset.
* @return A pair of IntegerVector arrays. Each array documents the indices of the collection that belong to it.
*/

std::pair<arma::uvec, arma::uvec> get_train_test_split_indices(int n, double train_size) {
    int train_n = static_cast<int>(n * train_size);

    arma::uvec indices = arma::shuffle(arma::regspace<arma::uvec>(0, n-1));

    arma::uvec train_indices = indices.head(train_n);
    arma::uvec test_indices = indices.tail(n - train_n);

    train_indices = arma::sort(train_indices);
    test_indices = arma::sort(test_indices);

    return std::make_pair(train_indices, test_indices);
}


std::unordered_map<std::string, arma::dmat> train_test_split_df(
    arma::dmat df,
    arma::uvec train_ix,
    arma::uvec test_ix
) {
    arma::dmat train_df = df.rows(train_ix);
    arma::dmat test_df = df.rows(test_ix);

    std::unordered_map<std::string, arma::dmat> result;
    result["train"] = train_df;
    result["test"] = test_df;

    return result;
}

std::unordered_map<std::string, arma::dvec> train_test_split_array(
    arma::dvec y,
    arma::uvec train_ix,
    arma::uvec test_ix
) {
    std::unordered_map<std::string, arma::dvec> result;
    result["train"] = y.elem(train_ix);
    result["test"] = y.elem(test_ix);
    return result;
}

std::pair<
    std::unordered_map<std::string, arma::dmat>,
    std::unordered_map<std::string, arma::dvec>
> train_test_split(arma::dmat x, arma::dvec y, double train_size = 0.7) {
    auto indices = get_train_test_split_indices(y.size(), train_size);
    auto x_split = train_test_split_df(x, indices.first, indices.second);
    auto y_split = train_test_split_array(y, indices.first, indices.second);
    return std::make_pair(x_split, y_split);
}
