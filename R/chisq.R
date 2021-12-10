#' Chi-Square Tests
#'
#' @noRd
get_chi_sq <- function(data, prediction) {
  num_treatments <- ncol(data) # nrow? depends ...
  chi_sq <- 0
  for (i in 1:num_treatments) {
    chi_sq <- chi_sq + chisq.test(data[, i], p = prediction[, i])$statistic # can we suppress the calculation of p values here?
  }
  p_value <- pchisq(chi_sq, df = num_treatments, lower.tail = FALSE) # DF: num of treatments, not -1
  return(c(chi_sq, p_value))
}


#' Chi-Square Tests for multiple theories
#'
#' @noRd
NULL
# get_all_chi_sq <- function(data, predictions) {
#   output <- c()
#   num_theories <- nrow(predictions)
#   for (i in 1:num_theories) {
#     output <- c(output, get_chi_sq(data, predictions[i,]))
#   }
#   return(output)
# }
