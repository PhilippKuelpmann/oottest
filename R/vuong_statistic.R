#' Getting the log-likelihood ratio
#'
#' @noRd
get_llr <- function(data, pred_I, pred_J) {
  # likelihood ratio of theory i and j
  result <- ExperimentalData * log(pred_I / pred_J)
  result <- sum(result)
  return(result)
}

#' Getting the variance of the log-likelihood ratio
#'
#' Only for 2 categories / binomial theories
#'
#' @noRd
get_variance_of_llr <- function(data, pred_I, pred_J) {
  # sum_{all t} n * p * (1-p) [log(p_i/pj) - log((1-p_i)/(1-p_j))]
  # numSubjects*probData*(1-probData)*(log(predI/predJ)-log((1-predI)/(1-predJ)))^2
  # data[,1]*data[,2] / (data[,1] + data[,2]) * (log(predI/predJ)-log((1-predI)/(1-predJ)))^2
  result <- data[, 1] * data[, 2] / (data[, 1] + data[, 2]) * (log(pred_I[, 1] / pred_J[, 1]) - log((pred_I[, 2]) / pred_J[, 2]))^2
  result <- sum(result)
  return(result)
}

#' Getting the variance of the log-likelihood ratio
#'
#' Only for 3 categories and broken right now
#'
#' @noRd
get_variance_of_llr_3 <- function(data, pred_I, pred_J) {
  # \sum limits_{t=1}^{10} (\sum \limits_{l=1}^3 log() ExperimentalData[1, t] log(PredictionI[1, t] / PredictionJ[1, t]) + ExperimentalData[2, t] log(PredictionI[2, t] / PredictionJ[2, t])
  # sum (all t) sum(all l) data * log(pred_I/pred_J)^2 - 2 * sum \limits_{l=1}^{2} \sum \limits_{m=l+1}^3 (data[l] * data[m]) / n  * log(pred_I[l]/pred_J[l]) log(pred_I[m]/pred_J[m])
  # TODO: Fix this!
  result <- sum(result)
  return(result)
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



# ExperimentalData <- matrix(data = 2, nrow = 4, ncol = 2, byrow=TRUE)
# PredictionI <- matrix(data = c(.5, .5, .6, .4, .7, .3, .8, .2), nrow = 4, ncol = 2, byrow=TRUE)
# PredictionJ <- matrix(data = c(.5, .5, .5, .5, .5, .5, .5, .5), nrow = 4, ncol = 2, byrow=TRUE)
