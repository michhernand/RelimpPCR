// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// normalize_vector
NumericVector normalize_vector(NumericVector x);
RcppExport SEXP _RelimpPCR_normalize_vector(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(normalize_vector(x));
    return rcpp_result_gen;
END_RCPP
}
// normalize_df
DataFrame normalize_df(DataFrame df);
RcppExport SEXP _RelimpPCR_normalize_df(SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(normalize_df(df));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _RelimpPCR_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// train_test_split_r
List train_test_split_r(DataFrame x, NumericVector y, double train_size);
RcppExport SEXP _RelimpPCR_train_test_split_r(SEXP xSEXP, SEXP ySEXP, SEXP train_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type train_size(train_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(train_test_split_r(x, y, train_size));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RelimpPCR_normalize_vector", (DL_FUNC) &_RelimpPCR_normalize_vector, 1},
    {"_RelimpPCR_normalize_df", (DL_FUNC) &_RelimpPCR_normalize_df, 1},
    {"_RelimpPCR_rcpp_hello_world", (DL_FUNC) &_RelimpPCR_rcpp_hello_world, 0},
    {"_RelimpPCR_train_test_split_r", (DL_FUNC) &_RelimpPCR_train_test_split_r, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_RelimpPCR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
