experimental_data <- matrix(10, nrow = 2, ncol = 8)
theory_1 <- matrix(c(.5, .5), nrow = 2, ncol = 8)
theory_2 <- matrix(c(.9, .1, .1, .9), nrow = 2, ncol = 8)

expect_equal(
  vuong_statistic(experimental_data, theory_1, theory_2),
  -vuong_statistic(experimental_data, theory_2, theory_1)
)

simple_data <- matrix(data = 1, nrow = 2, ncol = 1, byrow = TRUE)
th_1 <- matrix(data = c(.5, .5), nrow = 2, ncol = 1, byrow = TRUE)
th_2 <- matrix(data = c(.9, .1), nrow = 2, ncol = 1, byrow = TRUE)

llr_by_hand <- log(.5 / .9) + log(.5 / .1)
var_by_hand <- 1 / 2 * (log(.5 / .9) - log(.5 / .1))^2
vuong_by_hand <- llr_by_hand / var_by_hand^(.5)

expect_equal(get_llr(simple_data, th_1, th_2), llr_by_hand)
expect_equal(get_variance_of_llr(simple_data, th_1, th_2), var_by_hand)
expect_equal(vuong_statistic(simple_data, th_1, th_2), vuong_by_hand)


# Vuong score with more data should be larger
expect_gte(abs(vuong_statistic(simple_data * 3, th_1, th_2)), abs(vuong_statistic(simple_data, th_1, th_2)))

# Test that input int
# expect_
