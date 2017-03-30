// Google C++ Style Guide: https://google.github.io/styleguide/cppguide.html

#define ARMA_DONT_PRINT_ERRORS

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;


//==============================================================================
//' create_sin_cos
//'
//' fit sin curves to atv data
//'
//' @param n_rows the number of rows in the atv image
//' @param n_curves the number of sin/cos curves to use
//'
//' @return fitting coefficients
//'
//' @useDynLib atv
//' @importFrom Rcpp evalCpp
//' @importFrom RcppParallel RcppParallelLibs
//'
//' @export
//'
// [[Rcpp::export]]
arma::mat create_sin_cos(int n_rows, int n_curves){

  int n_cols = 2 * n_curves + 1;
  arma::mat x = arma::mat(n_rows, n_cols);

  double cycle = 2 * M_PI - (2 * M_PI) / n_rows;

  // set up model matrix
  for (int i = 0; i < n_curves; i++) {
    x.col(i) = sin(arma::linspace<arma::colvec>(0, (i + 1) * cycle, n_rows));
    x.col(i + n_curves) = cos(arma::linspace<arma::colvec>(0, (i + 1) * cycle, n_rows));
  }
  x.col(n_cols-1).fill(1.0);

  return(x);
}



struct sin_fit_ols_worker : public Worker {

  const arma::mat y;        // source
  const arma::mat x;        // source
  arma::mat& output;        // destination (pass by reference)

  // initialize with source and destination
  sin_fit_ols_worker(const arma::mat y,
                     const arma::mat x,
                     arma::mat& output)
    : y(y), x(x), output(output) {}

  // function call operator that iterates by slice
  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      arma::uvec sub_vec = find_finite(y.col(i));
      arma::vec z = y.col(i);
      int n_coefs = x.n_cols;

      if (sub_vec.size() > n_coefs + 1) {
        output.col(i) = arma::solve(x.rows(sub_vec), z.rows(sub_vec), arma::solve_opts::fast);
      } else {

        arma::colvec na_vec(n_coefs);
        na_vec.fill(NA_REAL);
        output.col(i) = na_vec;
      }

    }
  }
};

//==============================================================================
//' sin_fit_ols_parallel
//'
//' fit sin curves to atv data.
//'
//' @param y atv matrix of data
//' @param n_curves the number of sin/cos curves to use
//'
//' @return fitting coefficients
//'
//' @export
//'
// [[Rcpp::export]]
arma::mat sin_fit_ols_parallel(arma::mat y, int n_curves) {

  y = y.t();
  int n_rows = y.n_rows;
  int n_cols = y.n_cols;
  int n_coefs = 2 * n_curves + 1;

  arma::mat x = create_sin_cos(n_rows, n_curves);
  arma::mat output = arma::mat(n_coefs, n_cols);

  sin_fit_ols_worker sfw(y, x, output);

  RcppParallel::parallelFor(0, n_cols, sfw);

  return(output.t());
}







/*** R

*/




