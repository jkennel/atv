context("sin_fit_ols_parallel")

n <- 288
cycle <- 2 * pi - ((2 * pi) / n)
cos_seq <- cos(seq(0, cycle, length.out = n))
y <- matrix(rep(cos_seq, each = 4), ncol = n)

test_that("sin_fit_ols_parallel works", {

  # test if version works with one row
  y <- matrix(rep(cos_seq, each = 1), ncol = n)
  expect_equal(sin_fit_ols_parallel(y, 300), matrix(NA_real_, ncol = 300 * 2 + 1))

  # test if version works with one row
  y <- matrix(rep(cos_seq, each = 1), ncol = n)
  expect_equal(sin_fit_ols_parallel(y, 1)[,1], 0)
  expect_equal(sin_fit_ols_parallel(y, 1)[,2], 1)
  expect_equal(sin_fit_ols_parallel(y, 1)[,3], 0)


  # test if version works with multiple rows
  y <- matrix(rep(cos_seq, each = 4), ncol = n)
  expect_equal(sin_fit_ols_parallel(y, 1)[,1], rep(0, 4))
  expect_equal(sin_fit_ols_parallel(y, 1)[,2], rep(1, 4))
  expect_equal(sin_fit_ols_parallel(y, 1)[,3], rep(0, 4))


  amp <- 8
  phase <- pi/4
  cos_seq <- cos(seq(0, cycle, length.out = n) - pi/2)
  sin_seq <- sin(seq(0, cycle, length.out = n))
  mid_seq <- cos(seq(0, cycle, length.out = n) + phase)
  y_1 <- matrix(rep(cos_seq, each = 4), ncol = n)
  y_2 <- matrix(rep(sin_seq, each = 4), ncol = n)
  y_3 <- matrix(rep(mid_seq, each = 1), ncol = n)

  # test if shifts are working
  expect_equal(sin_fit_ols_parallel(y_1, 1), sin_fit_ols_parallel(y_2, 1))

  # check if phase amplitude works
  expect_equal(convert_to_amplitude_phase(sin_fit_ols_parallel(y_3*amp, 1))[1, 1], amp)
  expect_equal(convert_to_amplitude_phase(sin_fit_ols_parallel(y_3*amp, 1))[1, 2], phase)
  expect_equal(convert_to_amplitude_phase(sin_fit_ols_parallel(y_3*amp, 1))[1, 3], 0)


  # test multiple curves with amplitude and shift
  amp_1 <- 1.5
  amp_2 <- 4
  phase_1 <- pi/3
  phase_2 <- pi/5
  cos_seq <- amp_1*cos(seq(0, cycle, length.out = n) + phase_1)
  sin_seq <- amp_2*cos(2*seq(0, cycle, length.out = n) + phase_2)
  y <- matrix(cos_seq, ncol = n) + matrix(sin_seq, ncol = n)

  expect_equal(convert_to_amplitude_phase(sin_fit_ols_parallel(y, 2)),
               matrix(c(amp_1, amp_2, phase_1, phase_2, 0.0), nrow = 1))


})
