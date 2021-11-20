experimental_data <- matrix(10, nrow = 8, ncol = 2, byrow = TRUE)
theory_1 <- matrix(c(.5, .5), nrow = 8, ncol = 2, byrow = TRUE)
theory_2 <- matrix(c(.9, .1, .1, .9), nrow = 8, ncol = 2, byrow = TRUE)

expect_equal(
  vuong_statistic(experimental_data, theory_1, theory_2),
  -vuong_statistic(experimental_data, theory_2, theory_1)
)


result_a <- vuong_statistic(
  data = matrix(data = 1, nrow = 1, ncol = 2, byrow = TRUE),
  pred_I = matrix(data = c(.5, .5), nrow = 1, ncol = 2, byrow = TRUE),
  pred_J = matrix(data = c(.9, .1), nrow = 1, ncol = 2, byrow = TRUE)
)

expect_equal(
  result_a,
  (log(.5 / .9) + log(0.5 / 0.1)) / (1 / 2 * (log(0.5 / 0.9) -
    log(0.5 / 0.1))^2)^(0.5)
)
