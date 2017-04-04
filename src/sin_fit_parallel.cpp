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
arma::mat create_sin_cos(int n_rows, int n_curves, bool intercept = true){

  if (n_rows <= 0 ) {
    stop("n_rows must be greater than 0");
  }
  if (n_curves <= 0) {
    stop("n_curves must be greater than 0");
  }

  int n_cols;

  if (intercept) {
    n_cols = 2 * n_curves + 1;
  } else {
    n_cols = 2 * n_curves;
  }

  arma::mat x = arma::mat(n_rows, n_cols);

  double cycle = 2 * M_PI - ((2 * M_PI) / n_rows);

  // set up model matrix
  for (int i = 0; i < n_curves; i++) {
    x.col(i) = sin(arma::linspace<arma::colvec>(0, (i + 1) * cycle, n_rows));
    x.col(i + n_curves) = cos(arma::linspace<arma::colvec>(0, (i + 1) * cycle, n_rows));
  }

  // use intercept in regression?
  if (intercept) {
    x.col(n_cols-1).fill(1.0);
  }

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


//==============================================================================
//' convert_to_amplitude_phase
//'
//' convert regression fit to amplitude and phase
//'
//' @param coefs coefficient matrix of data
//'
//' @return phase and amplitude
//'
//' @export
//'
// [[Rcpp::export]]
arma::mat convert_to_amplitude_phase(arma::mat coefs) {

  arma::mat output = arma::mat(coefs.n_rows, coefs.n_cols);
  int n_curves = coefs.n_cols / 2;

  for (unsigned i = 0; i < n_curves; i++) {

    output.col(i) = arma::sqrt(arma::pow(coefs.col(i), 2) + arma::pow(coefs.col(i+n_curves), 2));
    output.col(i + n_curves) = -arma::atan2(coefs.col(i), coefs.col(i+n_curves));

  }
  output.col(coefs.n_cols-1) = coefs.col(coefs.n_cols-1);

  return(output);

}

// //==============================================================================
// //' amplitude_phase_adjust_approx
// //'
// //' adjust curves based on regression stats (approximate - only whole pixel shifts allowed) (faster and simpler code)
// //'
// //' @param coefs matrix of data
// //' @param n number of values per trace
// //' @param intercept whether to include an intercept
// //'
// //' @return phase and amplitude
// //'
// //' @export
// //'
// // [[Rcpp::export]]
// arma::mat amplitude_phase_adjust_approx(arma::mat coefs, int n, bool intercept=true) {
//
//   double cycle = 2 * M_PI - (2 * M_PI / n);
//   arma::rowvec cos_seq = cos(arma::linspace<arma::rowvec>(0, cycle, n));
//   int n_coefs = coefs.n_cols;
//   int n_trace = coefs.n_rows;
//   int n_curves = n_coefs / 2;
//
//   arma::rowvec coefs_row(n_coefs);
//   arma::mat curves(n_curves, n);
//
//   arma::mat output = coefs.col(0) * cos_seq;
//
//   int sh = 0;
//
//   for (int i; i < n_trace; i++) {
//     if (arma::is_finite(coefs.col(1)(i))){
//
//       sh = round((coefs.col(1)(i) / (2*M_PI)) * n);
//
//       if (intercept) {
//         output.row(i) = shift(output.row(i), -sh) + coefs.col(2)(i);
//       } else {
//         output.row(i) = shift(output.row(i), -sh);
//       }
//
//     }
//   }
//
//   return(output);
//
// }


//==============================================================================
//' amplitude_phase_adjust
//'
//' adjust curves based on regression stats
//'
//' @param coefs matrix of data
//' @param n number of values per trace
//' @param intercept whether to include an intercept
//'
//' @return phase and amplitude
//'
//' @export
//'
// [[Rcpp::export]]
arma::mat amplitude_phase_adjust(arma::mat coefs, int n, bool intercept) {

  double cycle = 2 * M_PI - (2 * M_PI / n);
  int n_coefs = coefs.n_cols;
  int n_trace = coefs.n_rows;
  int n_curves = n_coefs / 2;

  arma::mat curves(n_curves, n);
  arma::mat output(n_trace, n);
  arma::rowvec adjust(n);
  arma::rowvec cos_seq = arma::linspace<arma::rowvec>(0, cycle, n);
  arma::rowvec coefs_row(n_coefs);

  output.zeros();

  for (int j = 0; j < n_trace; j++) {

    adjust.zeros();
    coefs_row = coefs.row(j);

    for (int i = 0; i < n_curves; i++) {
      adjust += coefs_row(i) * cos( cos_seq * (i+1) + (coefs_row(i+n_curves)));
    }

    if (intercept){
      output.row(j) = adjust + coefs_row(n_coefs-1);
    } else {
      output.row(j) = adjust;
    }
  }
  return(output);
}

/*** R
# 288 = 2* pi
#val / 2*pi * 288
# coefs <- matrix(1:63, ncol=7)
# test_mult(coefs=coefs, n=20, intercept = FALSE)

# system.time(
#   tmp <- amplitude_phase_adjust(coefs=pa, n=288, intercept = FALSE)
# )
#
# plot(tt_mat[19000,], type='l')
# points(tmp[19000,], type='l', col='red')
#

#test_mult(1:10)

#
# system.time(
#
#   {
#   tt_filt <- rectangular_filter(tt_mat, k_h = 3, k_v = 3)
#   fit     <- sin_fit_ols_parallel(tt_filt, 1)
#   pa      <- convert_to_amplitude_phase(fit)
#   #tt_adj  <- amplitude_phase_adjust(pa, ncol(tt_mat), FALSE)
#   tt_adj2  <- amplitude_phase_adjust2(pa, ncol(tt_mat), FALSE)
#
#   na.approx(pa[,1])
#   na.approx(pa[,2])
#   na.approx(pa[,3])
#
#   tt_adj[is.na(tt_adj)] <- 0
#   tt_dif  <- tt_mat - tt_adj
#   }
#
# )
# rst <- raster(tt_mat - tt_adj)
# plot(rst,
#      asp = NA,
#      legend = FALSE,
#      col = color_scale(100),
#      zlim = c(-3, 3),
#      nr = 1)
#
# n <- 18200
# plot(tt_adj[n,], type='l')
# points(tt_adj2[n,], type='l', col='red')
#
# calc_r2 <- function(a, b){
#   a <- a - mean(a, na.rm = TRUE)
#   R2 <- 1 - sum(a^2)/sum((na.omit(b) - mean(b, na.rm = TRUE))^2)
# }
#
# r2 <- numeric(nrow(tt_mat))
# a <- tt_mat-tt_adj
#
# for (i in 1:nrow(tt_mat)) {
#   r2[i] <- calc_r2(a[i,], tt_mat[i,])
# }
# which.min(r2)
# par(new=TRUE)
# plot(rowMeans(tt_mat, na.rm=TRUE), col='red', type='l')
#
# plot(tt_dif[7200,])
# plot(tt_mat[7200,])
#
# plot(as.numeric((tt_mat)[31000:31004,]), type='l')
#
#
# points((tt_adj)[n,], type='l', col='red')
# points((tt_mat - tt_adj)[n,] + 1140, type='l', col='blue')
#
#
# plot(rowMeans(t(tt_mat), na.rm=TRUE))
# plot(rowMeans(t(tt_mat - tt_adj), na.rm=TRUE))
#
# rst <- raster(tt_mat - tt_adj)
# plot(rst,
#      asp = NA,
#      legend = FALSE,
#      col = color_scale(100),
#      zlim = c(-3, 3),
#      nr = 1)
#
# image(tt_filt - tt_adj)
#
# aa <- rowMeans(tt_mat)
# tt_mat[aa > 1140,] <- NA_real_
# amp_mat[aa > 1140,] <- NA_real_
# tt_mat[tt_mat > 1300] <- NA_real_
#
# tt_mat[amp_mat < 4000] <- NA_real_
# max(tt_mat,na.rm=TRUE)
#
# system.time(
#   aa <- rectangular_filter(tt_mat, k_v = 5, k_h = 7)
# )
#
# pdf()
# for(n in 1:nrow(tt_mat)){
#   if(!all(is.na(aa[n,]))){
#
#     plot(aa[n,], type='l', main=n)
#     #points(tt_mat[n,], type='l', col='red')
#   }
# }
# dev.off()
#
#
# plot(aa[20603,], type='l', main=n)
#
#
# #points(tt_mat[n,], type='l', col='red')
#
# qqnorm(aa[n,]-tt_mat[n,])
#
# points()
# # power_spectral_density_fast(tt_mat[5001,], 1:2)
# #
# #
# #
# #
# plot(tt_m[9000,], type='l')
# points(test[9000,], col='red', type='l')
# #points(1170.03125 + 64.56281 * cos(seq(0, 2*pi-2*pi/288, length.out=288) + 2.9479), col='red', type='l')
# # image(test)
# #2 * M_PI - (2 * M_PI) / n
*/




