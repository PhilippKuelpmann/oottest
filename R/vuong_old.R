#' Getting the variance of the log-likelihood ratio (2 categories)
#'
#' Only for 2 categories / binomial theories
#'
#' @noRd
get_variance_of_llr_2 <- function(data, pred_I, pred_J) {
  result <- data[, 1] * data[, 2] / (data[, 1] + data[, 2]) * (log(pred_I[, 1] / pred_J[, 1]) - log((pred_I[, 2]) / pred_J[, 2]))^2
  result <- sum(result)
  return(result)
}

#' Getting the variance of the log-likelihood ratio (3 categories)
#' pred[action, treatment]
#' Only for 3 categories
#'
#' @noRd
get_variance_of_llr_3 <- function(data, pred_I, pred_J) {
  treatments <- ncol(data)
  n <- colSums(data)
  output <- rep(0, treatments)
  for (t in 1:treatments) {
    output[t] <- output[t] + sum(data[, t] * log(pred_I[, t] / pred_J[, t])^2) # summing over all actions
    for (l in 1:2) {
      for (m in (l + 1):3) {
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
vuong_statistic_2 <- function(data, pred_I, pred_J) {
  result <- get_llr(data, pred_I, pred_J) / get_variance_of_llr_2(data, pred_I, pred_J)^(.5)
  return(result)
}

#' Implementation of the Vuong test for our setting 3x3
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param pred_I Model prediction for a theory (matrix of doubles/data.frame): columns are prediction of playing action 1,2,3..., rows are treatments
#' @param pred_J Model prediction for a theory (matrix of doubles/data.frame): columns are prediction of playing action 1,2,3..., rows are treatments
#'
#' @return The z score of testing theory I against theory J, given the data
#'
#' @examples (missing)
#' @export
vuong_statistic_3 <- function(data, pred_I, pred_J) {
  llr <- get_llr(data, pred_I, pred_J)
  var <- get_variance_of_llr_3(data, pred_I, pred_J)
  if (llr == 0) { # TODO: exactly zero?
    result <- 0
  } else {
    result <- - llr/var^(.5)
  }
  return(result)
}

#' Getting a vuong matrix
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param theories Model predictions for all theories. (how to structure? List of matrices?)
#'
#' @return Table of z-scores for all comparisons
#'
#' @examples (missing)
#' @export
vuong_matrix_2 <- function(data, theories) {
  num_theories <- length(theories)
  result <- matrix(, nrow = num_theories, ncol = num_theories)
  for (i in 1:num_theories) {
    for (j in 1:num_theories) {
      result[i, j] <- vuong_statistic_2(data, theories[[i]], theories[[j]])
    }
  }
  return(result)
}

#' Create a Vuong Matrix (for 3 actions)
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param theories Model predictions for all theories. (how to structure? List of matrices?)
#'
#' @return Table of z-scores for all comparisons
#'
#' @examples (missing)
#' @export
vuong_matrix_3 <- function(data, theories) {
  num_theories <- length(theories)
  result <- matrix(, nrow = num_theories, ncol = num_theories)
  for (i in 1:num_theories) {
    for (j in 1:num_theories) {
      result[i, j] <- vuong_statistic_3(data, theories[[i]], theories[[j]])
    }
  }
  return(result)
}
