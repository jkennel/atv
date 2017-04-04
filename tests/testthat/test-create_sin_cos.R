context("create_sin_cos")

cycle <- 2 * pi - ((2 * pi) / 10)

test_that("create_sin_cos works", {
  expect_equal(create_sin_cos(10, 1)[, 1],
               sin(seq(0, cycle, length.out = 10)))

  expect_equal(create_sin_cos(10, 1)[, 2],
               cos(seq(0, cycle, length.out = 10)))

  expect_equal(create_sin_cos(10, 1)[, 3],
               rep(1, 10))

  expect_equal(create_sin_cos(10, 2)[, 1],
               sin(seq(0, cycle, length.out = 10)))

  expect_equal(create_sin_cos(10, 2)[, 2],
               sin(seq(0, 2 * cycle, length.out = 10)))

  expect_equal(create_sin_cos(10, 2)[, 3],
               cos(seq(0, cycle, length.out = 10)))

  expect_equal(create_sin_cos(10, 2)[, 4],
               cos(seq(0, 2*cycle, length.out = 10)))

  # no intercept
  expect_equal(ncol(create_sin_cos(10, 2, intercept = FALSE)), 4)

  expect_equal(ncol(create_sin_cos(10, 10)), 21)
  expect_equal(nrow(create_sin_cos(10, 10)), 10)


  expect_error(create_sin_cos(0, 1))
  expect_error(create_sin_cos(-1, 1))
  expect_error(create_sin_cos(1, 0))
  expect_error(create_sin_cos(1, -1))


})
