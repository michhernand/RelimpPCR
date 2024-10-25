#define ARMA_64BIT_WORD
#include <armadillo>
#include <tuple>

std::tuple<arma::dvec, double, double> normalize_vector(
    arma::dvec x
) {
    double mean = arma::mean(x);
    double sd = arma::stddev(x);
    return std::make_tuple(
        (x - mean) / sd,
        mean,
        sd
    );
}

arma::dvec normalize_vector_pred(
    arma::dvec x,
    double mean,
    double sd
) {
    return (x - mean) / sd;
}

std::pair<
    arma::dmat,
    std::vector<
        std::pair<
            double,
            double
        >
    >
> normalize_df(arma::dmat df) {
    const size_t n = df.n_cols;
    std::vector<
        std::pair<
            double,
            double
        >
    > params;
    params.reserve(df.n_cols);

    for (size_t i = 0; i < n; i++) {
        std::tuple<
            arma::dvec,
            double,
            double
        > norm = normalize_vector(df.col(i));

        std::pair<
            double,
            double
        > array_payload = std::make_pair(
            std::get<1>(norm),
            std::get<2>(norm)
        );

        params[i] = array_payload;
        df.col(i) = std::get<0>(norm);
    }
    return std::make_pair(df, params);
}

arma::dmat normalize_df_pred(
    arma::dmat df,
    std::vector<
        std::pair<
            double,
            double
        >
    > params
) {
    if (df.n_cols != params.size()) {
        throw std::runtime_error(
            "n_col (" +
            std::to_string(df.n_cols) +
            ") must match vec len (" +
            std::to_string(params.size()) +
            ")"
        );
    }

    for (size_t i = 0; i < df.n_cols; i++) {
        auto param = params[i];
        df.col(i) = (df.col(i) - param.first) / param.second;
    }
    return df;
}
