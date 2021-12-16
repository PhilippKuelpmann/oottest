#' Chi-Square Tests
#'
#' @noRd
get_chi_sq <- function(data, prediction) {
  num_treatments <- ncol(data)
  chi_sq <- 0
  for (i in 1:num_treatments) {
    chi_sq <- chi_sq + stats::chisq.test(data[, i], p = prediction[, i])$statistic # can we suppress the calculation of p values here?
  }
  p_value <- stats::pchisq(chi_sq, df = num_treatments, lower.tail = FALSE) # DF: num of treatments, not -1
  output <- c(chi_sq, p_value)
  names(output) <- c("chi-sq", "p-value")
  return(output)
}


#' Chi-Square Tests for multiple theories
#'
#' @export
get_all_chi_sq <- function(data, predictions) {
  output <- c()
  num_theories <- dim(predictions)[3]
  for (i in 1:num_theories) {
    output <- rbind(output, get_chi_sq(data, predictions[, , i]))
  }
  rownames(output) <- colnames(predictions_two_action_games[1, , ])
  output[, "chi-sq"] <- round(output[, "chi-sq"], 2)
  output[, "p-value"] <- round(output[, "p-value"], 12)
  return(output)
}
