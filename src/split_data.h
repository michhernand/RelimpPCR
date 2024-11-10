
#ifndef SPLIT_DATA_H
#define SPLIT_DATA_H

#include <string>
#include <armadillo>
#include <unordered_map>

class SplitData {
public:
    std::unordered_map<std::string, arma::dmat> x;
    std::unordered_map<std::string, arma::dvec> y;

    SplitData(
        const std::unordered_map<std::string, arma::dmat>& x_data,
        const std::unordered_map<std::string, arma::dvec>& y_data
    ) : x(x_data), y(y_data) {};
};



#endif
