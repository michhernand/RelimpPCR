#define ARMA_64BIT_WORD
#include <armadillo>
#include <utility>
#include <unordered_map>

class SplitData {
    public:
        std::unordered_map<std::string, arma::dmat> x;
        std::unordered_map<std::string, arma::dvec> y;
        SplitData(
            const std::unordered_map<std::string, arma::dmat> x,
            const std::unordered_map<std::string, arma::dvec> y
        ) : x(x), y(y) {}
};

/**
* @brief Splits a dataset into train and test portions.
* @param n The size of the collection to be split.
* @param train_size The proprortion to allocate to the train.
* @return A pair of vec listing indices of train/test.
*/
std::pair<arma::uvec, arma::uvec> get_train_test_split_indices(arma::uword n, double train_size) {
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

SplitData train_test_split(const arma::dmat& x, const arma::dvec& y, double train_size) {
    arma::uword nn = y.n_elem;
    std::pair<arma::uvec, arma::uvec> indices = get_train_test_split_indices(nn, train_size);

    std::unordered_map<std::string, arma::dmat> x_split = train_test_split_df(x, indices.first, indices.second);
    std::unordered_map<std::string, arma::dvec> y_split = train_test_split_array(y, indices.first, indices.second);

    return SplitData(x_split, y_split);
}
