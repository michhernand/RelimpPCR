#include <armadillo>
#include "column_contribution.h"


void ColumnContribution::set_at_with_column(double val, arma::uword index) {
        this->r_squared_with_column[index] = val;
}

void ColumnContribution::set_at_without_column(double val, arma::uword index) {
        this->r_squared_without_column[index] = val;
}

ColumnContribution::ColumnContribution(
                arma::uword column,
                arma::uword n
                ) : 
        column(column),

        // Preallocates both vectors with zeros.
        r_squared_with_column(arma::dvec(n, arma::fill::zeros)),
        r_squared_without_column(arma::dvec(n, arma::fill::zeros)),

        // Default i to 0.
        iteration(0) 

{}

arma::uword ColumnContribution::get_column() {
        return this->column;
}

void ColumnContribution::set_at(double with_val, double without_val, arma::uword index) {
        this->set_at_with_column(with_val, index);
        this->set_at_without_column(without_val, index);
}


void ColumnContribution::set_next(double with_val, double without_val) {
        if (this->iteration >= this->r_squared_without_column.size()) {
                throw std::runtime_error("column contribution out of range");
        }

        this->set_at_with_column(with_val, this->iteration);
        this->set_at_without_column(without_val, this->iteration);
        this->iteration++;
}

double ColumnContribution::get_mean_with_column() {
        return arma::mean(this->r_squared_with_column);
};

double ColumnContribution::get_mean_without_column() {
        return arma::mean(this->r_squared_without_column);
};

/**
 * @brief This provides the average added benefit (i.e. relative importance) of including this feature.
 * @return The net r-squared.
 */
double ColumnContribution::get_lift() {
        return this->get_mean_with_column() - this->get_mean_without_column();
}
