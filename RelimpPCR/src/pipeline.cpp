// #include "Rcpp.h"
// #include "train_test_split.h"
// #include <unordered_map>

// Rcpp::NumericMatrix RelimpPCR(
//     Rcpp::DataFrame x,
//     Rcpp::NumericVector y,
//     double train_size
// ) {
//     auto split_data = train_test_split(x, y, train_size);
//     std::unordered_map<std::string, Rcpp::DataFrame> x_split = split_data.first;
//     std::unordered_map<std::string, Rcpp::NumericVector> y_split = split_data.second;
// }
