#ifndef COLUMN_CONTRIBUTION_H
#define COLUMN_CONTRIBUTION_H
#include <armadillo>

class ColumnContribution {
  private:
    arma::dvec r_squared_with_column;
    arma::dvec r_squared_without_column;
    arma::uword i;

    void set_at_with_column(double val, arma::uword i);
    void set_at_without_column(double val, arma::uword i);
  public:
    arma::uword column;

    ColumnContribution(arma::uword column, arma::uword n);

    void set_at(double with_val, double without_val, arma::uword i);
    void set_next(double with_val, double without_val);
    double get_mean_with_column();
    double get_mean_without_column();
    double get_lift();
};

#endif //COLUMN_CONTRIBUTION_H
