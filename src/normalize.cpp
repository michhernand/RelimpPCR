#define ARMA_64BIT_WORD
#include <armadillo>
#include <tuple>
#include "split_data.h"

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
    params.resize(df.n_cols);

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

SplitData normalize(
    SplitData split_data
) {
    std::unordered_map<std::string, arma::dmat> x_split = split_data.x;
    std::unordered_map<std::string, arma::dvec> y_split = split_data.y;

    // X-TRAIN
    std::pair<
        arma::dmat,
        std::vector<
            std::pair<
                double,
                double
            >
        >
    > x_train_norm_payload = normalize_df(x_split["train"]);

    // X-TEST
    arma::dmat x_test_norm = normalize_df_pred(
        x_split["test"],
        x_train_norm_payload.second
    );

    // X ALL
    std::unordered_map<std::string, arma::dmat> x_norm;
    x_norm["train"] = x_train_norm_payload.first;
    x_norm["test"] = x_test_norm;

    // Y-TRAIN
    std::tuple<
        arma::dvec, 
        double, 
        double
    > y_train_norm_payload = normalize_vector(y_split["train"]);

    // Y-TEST
    arma::dvec y_test_norm = normalize_vector_pred(
        y_split["test"],
        std::get<1>(y_train_norm_payload),
        std::get<2>(y_train_norm_payload)
    );

    // Y ALL
    std::unordered_map<std::string, arma::dvec> y_norm;
    y_norm["train"] = std::get<0>(y_train_norm_payload);
    y_norm["test"] = y_test_norm;

    return SplitData(x_norm, y_norm);
}
