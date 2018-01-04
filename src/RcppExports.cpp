// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// ll_pen_nb
double ll_pen_nb(arma::vec beta, arma::sp_mat X, arma::vec y, arma::vec offset, double theta, double lambda, arma::sp_mat S, double ll_factor, double lambda_factor, int n);
RcppExport SEXP _fastGenoGAM_ll_pen_nb(SEXP betaSEXP, SEXP XSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP thetaSEXP, SEXP lambdaSEXP, SEXP SSEXP, SEXP ll_factorSEXP, SEXP lambda_factorSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type S(SSEXP);
    Rcpp::traits::input_parameter< double >::type ll_factor(ll_factorSEXP);
    Rcpp::traits::input_parameter< double >::type lambda_factor(lambda_factorSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(ll_pen_nb(beta, X, y, offset, theta, lambda, S, ll_factor, lambda_factor, n));
    return rcpp_result_gen;
END_RCPP
}
// gr_ll_pen_nb
arma::vec gr_ll_pen_nb(arma::vec beta, arma::sp_mat X, arma::sp_mat XT, arma::vec y, arma::vec offset, double theta, double lambda, arma::sp_mat S);
RcppExport SEXP _fastGenoGAM_gr_ll_pen_nb(SEXP betaSEXP, SEXP XSEXP, SEXP XTSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP thetaSEXP, SEXP lambdaSEXP, SEXP SSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type XT(XTSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type S(SSEXP);
    rcpp_result_gen = Rcpp::wrap(gr_ll_pen_nb(beta, X, XT, y, offset, theta, lambda, S));
    return rcpp_result_gen;
END_RCPP
}
// compute_pen_hessian
arma::sp_mat compute_pen_hessian(arma::vec beta, arma::sp_mat X, arma::sp_mat XT, arma::vec offset, arma::vec y, arma::sp_mat S, double lambda, double theta);
RcppExport SEXP _fastGenoGAM_compute_pen_hessian(SEXP betaSEXP, SEXP XSEXP, SEXP XTSEXP, SEXP offsetSEXP, SEXP ySEXP, SEXP SSEXP, SEXP lambdaSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type XT(XTSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type S(SSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_pen_hessian(beta, X, XT, offset, y, S, lambda, theta));
    return rcpp_result_gen;
END_RCPP
}
// compute_stdError
arma::sp_mat compute_stdError(arma::sp_mat X, arma::sp_mat H);
RcppExport SEXP _fastGenoGAM_compute_stdError(SEXP XSEXP, SEXP HSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type H(HSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_stdError(X, H));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fastGenoGAM_ll_pen_nb", (DL_FUNC) &_fastGenoGAM_ll_pen_nb, 10},
    {"_fastGenoGAM_gr_ll_pen_nb", (DL_FUNC) &_fastGenoGAM_gr_ll_pen_nb, 8},
    {"_fastGenoGAM_compute_pen_hessian", (DL_FUNC) &_fastGenoGAM_compute_pen_hessian, 8},
    {"_fastGenoGAM_compute_stdError", (DL_FUNC) &_fastGenoGAM_compute_stdError, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_fastGenoGAM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}