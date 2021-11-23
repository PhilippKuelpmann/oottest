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
    output[t] <- output[t] + sum(data[, t] * log(pred_I[, t] / pred_J[, t])^2) # summing over all actions
    for (l in 1:(actions-1)) {
      for (m in (l + 1):actions) {
        output[t] <- output[t] - 2 * (data[l, t] * data[m, t]) / n[t] * log(pred_I[l, t] / pred_J[l, t]) * log(pred_I[m, t] / pred_J[m, t])
      }
    }
  }
  return(sum(output))
}

#' Implementation of the Vuong test for our setting
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param pred_I Model prediction for a theory (matrix of doubles/data.frame): columns are prediction of playing action 1,2,3..., rows are treatments
#' @param pred_J Model prediction for a theory (matrix of doubles/data.frame): columns are prediction of playing action 1,2,3..., rows are treatments
#'
#' @return The z score of testing theory I against theory J, given the data
#'
#' @examples (missing)
#' @export
vuong_statistic <- function(data, pred_I, pred_J) {
  result <- get_llr(data, pred_I, pred_J) / get_variance_of_llr(data, pred_I, pred_J)^(.5)
  return(result)
}


#' Getting a vuong matrix
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param theories Model predictions for all theories. (how to structure? array? data table? list of matrices?)
#'
#' @return Table of z-scores for all comparisons
#'
#' @examples (missing)
#' @export
vuong_matrix <- function(data, theories) {
  num_theories <- length(theories)
  result <- matrix(, nrow = num_theories, ncol = num_theories)
  for (i in 1:num_theories) {
    for (j in 1:num_theories) {
      result[i, j] <- vuong_statistic(data, theories[[i]], theories[[j]])
    }
  }
  # TODO: Name cols and rows if the original theory has names
  return(result)
}
