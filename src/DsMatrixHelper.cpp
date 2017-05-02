#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericMatrix gamma_helper(NumericMatrix Sr, NumericVector ar, NumericMatrix Or) {

  arma::mat S = Rcpp::as<arma::mat>(Sr);
  arma::vec a = Rcpp::as<arma::vec>(ar);
  arma::mat O = Rcpp::as<arma::mat>(Or);
  int R = a.n_rows;
  int J = S.n_rows;

  arma::mat G = arma::zeros<arma::mat>(J,J);

  for(int i = 0; i<R; i++) {
    G = G + ( (a[i] * ( S.col(i) * S.col(i).t() )) % O);
  }

  G = G * (1/ ((float) R));

  return Rcpp::wrap(G);
}

