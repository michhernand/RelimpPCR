#include <armadillo>
#ifdef CLANGD_CONFIG_LOADED
#pragma message("Clangd config.yaml is being used!")
#endif

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
                arma::uword i;

                void set_at_with_column(double val, arma::uword index) {
                        this->r_squared_with_column[index] = val;
                }

                void set_at_without_column(double val, arma::uword index) {
                        this->r_squared_without_column[index] = val;
                }
        public:
                // The index of the column.
                arma::uword column;

                /**
                * @brief Preallocates arrays with 'n' values.
                * @param column Index of the column being tracked.
                * @param n The number of r_squared iterations being tracked.
                */
                ColumnContribution(
                        arma::uword column,
                        arma::uword n
                ) : 
                        column(column),

                        // Preallocates both vectors with zeros.
                        r_squared_with_column(arma::dvec(n, arma::fill::zeros)),
                        r_squared_without_column(arma::dvec(n, arma::fill::zeros)),

                        // Default i to 0.
                        i(0) 

                        {}


                void set_at(double with_val, double without_val, arma::uword index) {
                        this->set_at_with_column(with_val, index);
                        this->set_at_without_column(without_val, index);
                }


                void set_next(double with_val, double without_val) {
                        if (this->i >= this->r_squared_without_column.size()) {
                                throw std::runtime_error("column contribution out of range");
                        }

                        this->set_at_with_column(with_val, this->i);
                        this->set_at_without_column(without_val, this->i);
                        this->i++;
                }

                double get_mean_with_column() {
                        return arma::mean(this->r_squared_with_column);
                };

                double get_mean_without_column() {
                        return arma::mean(this->r_squared_without_column);
                };

    /**
    * @brief This provides the average added benefit (i.e. relative importance) of including this feature.
    * @return The net r-squared.
    */
    double get_lift() {
      return this->get_mean_with_column() - this->get_mean_without_column();
    }
};
