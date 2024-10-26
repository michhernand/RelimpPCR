// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// split_and_normalize_r
Rcpp::List split_and_normalize_r(Rcpp::NumericMatrix x, Rcpp::NumericVector y, double train_size);
RcppExport SEXP _RelimpPCR_split_and_normalize_r(SEXP xSEXP, SEXP ySEXP, SEXP train_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type train_size(train_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(split_and_normalize_r(x, y, train_size));
    return rcpp_result_gen;
END_RCPP
}
// RelimpPCR
int RelimpPCR(Rcpp::NumericMatrix x, Rcpp::NumericVector y, double train_size);
RcppExport SEXP _RelimpPCR_RelimpPCR(SEXP xSEXP, SEXP ySEXP, SEXP train_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type train_size(train_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(RelimpPCR(x, y, train_size));
    return rcpp_result_gen;
END_RCPP
}
// train_test_split_r
Rcpp::List train_test_split_r(Rcpp::NumericMatrix x, Rcpp::NumericVector y, double train_size);
RcppExport SEXP _RelimpPCR_train_test_split_r(SEXP xSEXP, SEXP ySEXP, SEXP train_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type train_size(train_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(train_test_split_r(x, y, train_size));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RelimpPCR_split_and_normalize_r", (DL_FUNC) &_RelimpPCR_split_and_normalize_r, 3},
    {"_RelimpPCR_RelimpPCR", (DL_FUNC) &_RelimpPCR_RelimpPCR, 3},
    {"_RelimpPCR_train_test_split_r", (DL_FUNC) &_RelimpPCR_train_test_split_r, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_RelimpPCR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
