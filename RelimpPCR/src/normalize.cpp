#include "Rcpp.h"
#include "Rcpp/DataFrame.h"
using namespace Rcpp;

double calc_mean(NumericVector x, int n) {
    double sum = 0.0;
    for (int i = 0; i < n; ++i) {
        sum += x[i];
    }
    return sum / n;
}

double calc_sd(NumericVector x, double mean, int n) {
    double sq_sum = 0.0;
    for (int i = 0; i < n; ++i) {
        sq_sum += (x[i] - mean) * (x[i] - mean);
    }
    return std::sqrt(sq_sum / (n - 1));
}

// [[Rcpp::export]]
NumericVector normalize_vector(NumericVector x) {
    int n = x.length();
    double mean = calc_mean(x, n);
    double sd = calc_sd(x, mean, n);

    for (int i = 0; i < n; ++i) {
        x[i] = (x[i] - mean) / sd;
    }
    return x;
}

// [[Rcpp::export]]
DataFrame normalize_df(DataFrame df) {
    int n = df.length();
    for (int i = 0; i < n; ++i) {
        df[i] = normalize_vector(df[i]);
    }
    return df;
}
