#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List train_test_split_df(DataFrame df, double train_size = 0.7) {
    Rcout << "Starting train_test_split_df" << std::endl;
    int n = df.nrows();
    Rcout << "Number of rows: " << n << std::endl;
    int train_n = static_cast<int>(n * train_size);
    Rcout << "Train size: " << train_n << std::endl;
    
    // Create a random permutation of row indices
    IntegerVector indices = seq(0, n - 1);
    Rcout << "Indices created" << std::endl;
    indices = sample(indices, n, false);
    Rcout << "Indices sampled" << std::endl;
    
    // Split the indices into train and test sets
    IntegerVector train_indices = indices[seq(0, train_n - 1)];
    Rcout << "Train indices created. Size: " << train_indices.size() << std::endl;
    IntegerVector test_indices = indices[seq(train_n, n - 1)];
    Rcout << "Test indices created. Size: " << test_indices.size() << std::endl;
    
    // Create train and test DataFrames
    Rcout << "Creating train DataFrame" << std::endl;
    List train_list(df.size());
    for (int i = 0; i < df.size(); i++) {
        train_list[i] = as<Vector<REALSXP>>(df[i])[train_indices];
    }
    DataFrame train_df(train_list);
    train_df.attr("names") = df.attr("names");
    train_df.attr("row.names") = train_indices + 1;
    Rcout << "Train DataFrame created" << std::endl;
    
    Rcout << "Creating test DataFrame" << std::endl;
    List test_list(df.size());
    for (int i = 0; i < df.size(); i++) {
        test_list[i] = as<Vector<REALSXP>>(df[i])[test_indices];
    }
    DataFrame test_df(test_list);
    test_df.attr("names") = df.attr("names");
    test_df.attr("row.names") = test_indices + 1;
    Rcout << "Test DataFrame created" << std::endl;
    
    Rcout << "Returning list" << std::endl;
    // Return as a list
    return List::create(Named("train") = train_df,
                        Named("test") = test_df);
}

// [[Rcpp::export]]
List train_test_split(DataFrame x, NumericVector y) {
    Rcout << "Starting train_test_split" << std::endl;
    Rcout << "DataFrame rows: " << x.nrows() << ", y length: " << y.size() << std::endl;
    return train_test_split_df(x);
}
