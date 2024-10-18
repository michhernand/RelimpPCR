#include "Rcpp.h"
#include <utility>
#include <unordered_map>
using namespace Rcpp;


/**
* @brief Documents how to perform a train/test split. Provides indices for elements in train and elements in test.
* @param n The size of the collection to be split.
* @param train_size The proprortion of the collection allocated to the training dataset.
* @return A pair of IntegerVector arrays. Each array documents the indices of the collection that belong to it.
*/
std::pair<Rcpp::IntegerVector, Rcpp::IntegerVector>get_train_test_split_indices(int n, double train_size) {
    int train_n = static_cast<int>(n * train_size);

    // Randomly select train_n indices for the training set
    Rcpp::IntegerVector indices = seq(0, n - 1);
    Rcpp::IntegerVector train_indices = Rcpp::sample(indices, train_n, false);

    // Invert the selection
    LogicalVector is_train(n);
    for (int i = 0; i < train_n; i++) {
        is_train[train_indices[i]] = true;
    }

    // Apply inverted selection for indices for the test set.
    Rcpp::IntegerVector test_indices = indices[!is_train];

    return std::make_pair(train_indices, test_indices);
}


std::unordered_map<std::string, Rcpp::DataFrame> train_test_split_df(DataFrame df, Rcpp::IntegerVector train_ix, Rcpp::IntegerVector test_ix) {
    List train_list(df.size());
    List test_list(df.size());
    for (int i = 0; i < df.size(); i++) {
        train_list[i] = as<Vector<REALSXP>>(df[i])[train_ix];
        test_list[i] = as<Vector<REALSXP>>(df[i])[test_ix];
    }

    DataFrame train_df(train_list);
    train_df.attr("names") = df.attr("names");
    train_df.attr("row.names") = train_ix + 1;

    DataFrame test_df(test_list);
    test_df.attr("names") = df.attr("names");
    test_df.attr("row.names") = test_ix + 1;

    std::unordered_map<std::string, Rcpp::DataFrame> result;
    result["train"] = train_df;
    result["test"] = test_df;
    return result;
}

std::unordered_map<std::string, Rcpp::NumericVector> train_test_split_array(NumericVector y, Rcpp::IntegerVector train_ix, Rcpp::IntegerVector test_ix) {
    std::unordered_map<std::string, Rcpp::NumericVector> result;
    result["train"] = y[train_ix];
    result["test"] = y[test_ix];
    return result;
}

std::pair<
    std::unordered_map<std::string, Rcpp::DataFrame>,
    std::unordered_map<std::string, Rcpp::NumericVector>
> train_test_split(Rcpp::DataFrame x, Rcpp::NumericVector y, double train_size = 0.7) {
    auto indices = get_train_test_split_indices(y.length(), train_size);
    auto x_split = train_test_split_df(x, indices.first, indices.second);
    auto y_split = train_test_split_array(y, indices.first, indices.second);
    return std::make_pair(x_split, y_split);
}

// [[Rcpp::export]]
List train_test_split_r(DataFrame x, NumericVector y, double train_size = 0.7) {
    auto result = train_test_split(x, y, train_size);
    std::unordered_map<std::string, Rcpp::DataFrame> x_result = result.first;
    std::unordered_map<std::string, Rcpp::NumericVector> y_result = result.second;

    return List::create(
        Named("x_train") = x_result["train"],
        Named("x_test") = x_result["test"],
        Named("y_train") = y_result["train"],
        Named("y_test") = y_result["test"]
    );
}
