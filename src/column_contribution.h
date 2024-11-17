#ifndef COLUMN_CONTRIBUTION_H
#define COLUMN_CONTRIBUTION_H
#include <armadillo>

/**
 * Tracks a column's contributions to r-squared over multiple models.
 */
class ColumnContribution {
        private:
                // Used to log r_squared for models that contain this column.
                arma::dvec r_squared_with_column;
                // Used to log r_squared for models that do not contain this column.
                arma::dvec r_squared_without_column;
                // Used to track which index is next.
                arma::uword iteration;

                void set_at_with_column(double val, arma::uword index);
                void set_at_without_column(double val, arma::uword index);
        public:
                arma::uword column;

                ColumnContribution(arma::uword column, arma::uword n);

                arma::uword get_column();
                void set_at(double with_val, double without_val, arma::uword index);
                void set_next(double with_val, double without_val);
                double get_mean_with_column();
                double get_mean_without_column();
                double get_lift();
};

#endif //COLUMN_CONTRIBUTION_H
