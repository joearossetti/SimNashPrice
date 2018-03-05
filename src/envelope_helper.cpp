#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List envelope_helper(NumericMatrix Sr, NumericVector ar) {

  arma::mat S = Rcpp::as<arma::mat>(Sr);
  arma::vec a = Rcpp::as<arma::vec>(ar);

  int R = a.n_rows; //number of individuals
  int J = S.n_rows; //number of products
  float R_inv = 1 / ((float) R);

  // loop through individuals and make the Dp_si and Dv_si
  arma::mat S_si(J, J);
  arma::mat S_si_t(J, J);

  arma::mat Alphai_p(J, J);
  arma::mat Alphai_v = -1 * arma::ones(J, J);
  Alphai_v.diag() = -Alphai_v.diag();

  arma::mat Dp_repd(J,J);
  arma::mat Dp_repd_t(J,J);
  arma::mat Dv_repd(J,J);
  arma::mat Dv_repd_t(J,J);

  arma::mat Dp_si(J, J);
  arma::mat Dv_si(J, J);
  arma::mat Dp_s = arma::zeros(J, J);
  arma::mat Dv_s = arma::zeros(J, J);

  arma::cube DpDp_si(J, J, J);
  arma::cube DvDv_si(J, J, J);
  arma::cube DvDp_si(J, J, J);
  arma::cube DpDv_si(J, J, J);
  arma::cube DpDp_s = arma::zeros(J, J, J);
  arma::cube DvDv_s = arma::zeros(J, J, J);
  arma::cube DvDp_s = arma::zeros(J, J, J);
  arma::cube DpDv_s = arma::zeros(J, J, J);

  arma::mat DpDp_repd(J,J);
  arma::mat DpDp_repd_t(J,J);
  arma::mat DpDv_repd(J,J);
  arma::mat DpDv_repd_t(J,J);
  arma::mat DvDp_repd(J,J);
  arma::mat DvDp_repd_t(J,J);
  arma::mat DvDv_repd(J,J);
  arma::mat DvDv_repd_t(J,J);

  arma::field<arma::cube> DpDpDp_si(J);
  arma::field<arma::cube> DpDpDp_s(J);
  arma::field<arma::cube> DvDvDp_si(J);
  arma::field<arma::cube> DvDvDp_s(J);
  arma::field<arma::cube> DpDvDp_si(J);
  arma::field<arma::cube> DpDvDp_s(J);
  arma::field<arma::cube> DvDpDp_s(J);
  arma::field<arma::cube> DvDpDp_si(J);

  for(int k = 0; k<J; k++) {
    DpDpDp_s(k) = arma::zeros(J, J, J);
    DvDvDp_s(k) = arma::zeros(J, J, J);
    DpDvDp_s(k) = arma::zeros(J, J, J);
    DvDpDp_s(k) = arma::zeros(J, J, J);
    DpDpDp_si(k) = arma::zeros(J, J, J);
    DvDvDp_si(k) = arma::zeros(J, J, J);
    DpDvDp_si(k) = arma::zeros(J, J, J);
    DvDpDp_si(k) = arma::zeros(J, J, J);
  }

  for(int i = 0; i<R; i++){
    // set up share matrices for individual i
    S_si = arma::repmat(S.col(i), 1, J);
    S_si_t = S_si.t();
    S_si_t.diag() = (1-S.col(i));

    // set up alphai_p matrices for individual i
    Alphai_p = -a[i] * Alphai_v;

    // first derivatives
    // ---------
    //Dp_si
    Dp_si = Alphai_p % S_si % S_si_t;

    //Dv_si
    Dv_si = Alphai_v % S_si % S_si_t;

    for(int k = 0; k<J; k++) {
      // second derivatives
      Dp_repd = arma::repmat(Dp_si.col(k), 1, J);
      Dv_repd = arma::repmat(Dv_si.col(k), 1, J);

      Dp_repd_t = Dp_repd.t();
      Dp_repd_t.diag() = -Dp_repd_t.diag();
      Dv_repd_t = Dv_repd.t();
      Dv_repd_t.diag() = -Dv_repd_t.diag();

      DpDp_si.slice(k) = Alphai_p % (Dp_repd % S_si_t + Dp_repd_t % S_si);
      DvDv_si.slice(k) = Alphai_v % (Dv_repd % S_si_t + Dv_repd_t % S_si);
      DvDp_si.slice(k) = Alphai_p % (Dv_repd % S_si_t + Dv_repd_t % S_si);
      DpDv_si.slice(k) = Alphai_v % (Dp_repd % S_si_t + Dp_repd_t % S_si);

      // third derivatives
      for(int j =0; j<J; j++) {
        DpDp_repd = arma::repmat(DpDp_si.slice(k).col(j), 1, J);
        DvDp_repd = arma::repmat(DvDp_si.slice(k).col(j), 1, J);
        DvDv_repd = arma::repmat(DvDv_si.slice(k).col(j), 1, J);
        DpDv_repd = arma::repmat(DpDv_si.slice(k).col(j), 1, J);
        DvDp_repd = arma::repmat(DvDp_si.slice(k).col(j), 1, J);

        DpDp_repd_t = Dp_repd.t();
        DpDp_repd_t.diag() = -Dp_repd_t.diag();
        DvDp_repd_t = Dp_repd.t();
        DvDp_repd_t.diag() = -Dv_repd_t.diag();
        DvDv_repd_t = Dp_repd.t();
        DvDv_repd_t.diag() = -Dv_repd_t.diag();
        DpDv_repd_t = Dp_repd.t();
        DpDv_repd_t.diag() = -Dv_repd_t.diag();
        DvDp_repd_t = Dp_repd.t();
        DvDp_repd_t.diag() = -Dv_repd_t.diag();

        DpDpDp_si(k).slice(j) = Alphai_p % (DpDp_repd % S_si_t + 2 * Dp_repd % Dp_repd_t + DpDp_repd_t % S_si);
        DvDvDp_si(k).slice(j) = Alphai_p % (DvDv_repd % S_si_t + 2 * Dv_repd % Dv_repd_t + DvDv_repd_t % S_si);
        DpDvDp_si(k).slice(j) = Alphai_p % (DpDv_repd % S_si_t + 2 * Dv_repd % Dv_repd_t + DpDv_repd_t % S_si);
        DvDpDp_si(k).slice(j) = Alphai_p % (DvDp_repd % S_si_t + 2 * Dp_repd % Dp_repd_t + DvDp_repd_t % S_si);
      }
    }
    // ---------


    // accumulate
    // ---------
    // first derivatives
    Dp_s = Dp_s + Dp_si * R_inv;
    Dv_s = Dv_s + Dv_si * R_inv;

    //second derivatives
    DpDp_s = DpDp_s + DpDp_si * R_inv;
    DpDv_s = DpDv_s + DpDv_si * R_inv;
    DvDp_s = DvDp_s + DvDp_si * R_inv;
    DvDv_s = DvDv_s + DvDv_si * R_inv;

    //third derivatives
    for(int k = 0; k<J; k++) {
      DpDpDp_s(k) = DpDpDp_s(k) + DpDpDp_si(k) * R_inv;
      DvDvDp_s(k) = DvDvDp_s(k) + DvDvDp_si(k) * R_inv;
      DpDvDp_s(k) = DpDvDp_s(k) + DpDvDp_si(k) * R_inv;
      DvDpDp_s(k) = DpDvDp_s(k) + DvDpDp_si(k) * R_inv;
    }

  }

  return Rcpp::List::create(Rcpp::Named("num_prods") = J,
                            Rcpp::Named("num_individuals") = R,
                            Rcpp::Named("Dp_s") = Dp_s, Rcpp::Named("Dv_s") = Dv_s,
                            Rcpp::Named("DpDp_s") = DpDp_s, Rcpp::Named("DpDv_s") = DpDv_s,
                            Rcpp::Named("DvDp_s") = DvDp_s , Rcpp::Named("DvDv_s") = DvDv_s,
                            Rcpp::Named("DpDpDp_s") = DpDpDp_s , Rcpp::Named("DvDvDp_s") = DvDvDp_s,
                            Rcpp::Named("DpDvDp_s") = DpDvDp_s,Rcpp::Named("DvDpDp_s") = DvDpDp_s);

  //return Rcpp::List::create(Rcpp::Named("num_prods") = J);
}
