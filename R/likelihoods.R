#' Getting a log likelihood
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param prediction Model prediction for one theory: rows/cols as data
#'
#' @return Table of z-scores for all comparisons
#'
#' @examples (missing)
#' @noRd
get_log_lh <- function(data, prediction, with_constant = FALSE) {
  # constant <- log(factorial(colSums(data))) - colSums(log(factorial(data)))
  output <- colSums(data * log(prediction))
  return(output)
}

#' Getting the log-likelihood ratio
#'
#' @noRd
get_llr <- function(data, pred_I, pred_J) {
  # likelihood ratio of theory i and j
  result <- data * log(pred_I / pred_J)
  result <- sum(result)
  return(result)
}

#' Getting the variance of the log-likelihood ratio
#' pred[action, treatment]
#'
#' @noRd
get_variance_of_llr <- function(data, pred_I, pred_J) {
  # sum(l=1...n) (log(pil/pjl))^2 n pl - 2 sum (l=1...n) sum(m=l+1...n) n log(pil/pjl)log(pim/pjm) pl pm
  # pl -> xl / n
  # => sum(l=1...n) (log(pil/pjl))^2 xl - 2 sum (l=1...n) sum(m=l+1...n) log(pil/pjl)log(pim/pjm) (xl xm) / n
  treatments <- ncol(data)
  actions <- nrow(data)
  n <- colSums(data)
  output <- rep(0, treatments)
  for (t in 1:treatments) {
    output[t] <- output[t] + sum(data[, t] * (n[t] - data[, t]) / n[t] * log(pred_I[, t] / pred_J[, t])^2) # summing over all actions
    for (l in 1:(actions - 1)) {
      for (m in (l + 1):actions) {
        output[t] <- output[t] - 2 * (data[l, t] * data[m, t]) / n[t] * log(pred_I[l, t] / pred_J[l, t]) * log(pred_I[m, t] / pred_J[m, t])
      }
    }
  }
  return(sum(output))
}
