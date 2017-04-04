context("amplitude_phase_adjust")

test_that("amplitude_phase_adjust works", {
  n <- 288
  amp_1 <- 1.5
  amp_2 <- 4
  phase_1 <- pi/3
  phase_2 <- pi/5
  cycle <- 2 * pi - ((2 * pi) / n)
  cos_seq <- amp_1*cos(seq(0, cycle, length.out = n) + phase_1)
  sin_seq <- amp_2*cos(2*seq(0, cycle, length.out = n) + phase_2)
  y <- matrix(cos_seq, ncol = n)

  # single curve
  expect_equal(amplitude_phase_adjust(convert_to_amplitude_phase(sin_fit_ols_parallel(y, 1)), n, TRUE), y)

  # multiple curves
  y <- matrix(cos_seq, ncol = n) + matrix(sin_seq, ncol = n)
  expect_equal(amplitude_phase_adjust(convert_to_amplitude_phase(sin_fit_ols_parallel(y, 2)), n, FALSE), y)

  # multiple curves with intercept
  expect_equal(amplitude_phase_adjust(convert_to_amplitude_phase(sin_fit_ols_parallel(y + 2, 2)), n, intercept = TRUE), y + 2)


})
