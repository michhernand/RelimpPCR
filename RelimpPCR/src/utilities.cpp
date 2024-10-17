#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List train_test_split_df(DataFrame df, double train_size = 0.7) {
    int n = df.nrows();
    int train_n = static_cast<int>(n * train_size);
    
    IntegerVector indices = seq(0, n - 1);
    indices = sample(indices, n, false);
    
    IntegerVector train_indices = indices[seq(0, train_n - 1)];
    IntegerVector test_indices = indices[seq(train_n, n - 1)];
    
    List train_list(df.size());
    for (int i = 0; i < df.size(); i++) {
        train_list[i] = as<Vector<REALSXP>>(df[i])[train_indices];
    }
    DataFrame train_df(train_list);
    train_df.attr("names") = df.attr("names");
    train_df.attr("row.names") = train_indices + 1;
    
    List test_list(df.size());
    for (int i = 0; i < df.size(); i++) {
        test_list[i] = as<Vector<REALSXP>>(df[i])[test_indices];
    }
    DataFrame test_df(test_list);
    test_df.attr("names") = df.attr("names");
    test_df.attr("row.names") = test_indices + 1;
    return List::create(Named("train") = train_df,
                        Named("test") = test_df);
}

// [[Rcpp::export]]
List train_test_split(DataFrame x, NumericVector y) {
    return train_test_split_df(x);
}
