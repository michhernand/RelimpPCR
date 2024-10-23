#include <armadillo>

arma::dvec normalize_vector(
    arma::dvec x
) {
    double mean = arma::mean(x);
    double sd = arma::stddev(x);
    return (x - mean) / sd;
}

arma::dmat normalize_df(arma::dmat df) {
    const size_t n = df.n_cols;
    for (size_t i = 0; i < n; i++) {
        df.col(i) = normalize_vector(df.col(i));
    }
    return df;
}
