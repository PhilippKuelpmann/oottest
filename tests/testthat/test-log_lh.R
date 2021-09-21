test_that("multiplication works", {
  expect_equal(log_lh(model_prediction=matrix(c(0,1,1,0), nrow=2),
                      data_experiment=matrix(c(0,14,14,14), nrow=2)
                      ),
               -0.693147180559945, tolerance=.Machine$double.eps)
})
