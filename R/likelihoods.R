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
  output <- colSums(data * log(prediction))
  return(output)
}

#' Getting the log-likelihood ratio
#'
#' @noRd
get_llr <- function(data, pred_i, pred_j) {
  # likelihood ratio of theory i and j
  result <- data * log(pred_i / pred_j)
  result <- sum(result)
  return(result)
}

#' Getting the variance of the log-likelihood ratio
#' pred[action, treatment]
#'
#' @noRd
get_variance_of_llr <- function(data, pred_i, pred_j) {
  treatments <- ncol(data)
  actions <- nrow(data)
  n <- colSums(data)
  output <- rep(0, treatments)
  for (t in seq_len(treatments)) {
    output[t] <- output[t] + sum(data[, t] * (n[t] - data[, t]) / n[t] * log(pred_i[, t] / pred_j[, t])^2) # summing over all actions
    for (l in 1:(actions - 1)) {
      for (m in (l + 1):actions) {
        output[t] <- output[t] - 2 * (data[l, t] * data[m, t]) / n[t] * log(pred_i[l, t] / pred_j[l, t]) * log(pred_i[m, t] / pred_j[m, t])
      }
    }
  }
  return(sum(output))
}
