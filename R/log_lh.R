#' Calculate log-likelihood for a theory
#'
#' @param model_prediction Model prediction for a theory (matrix of doubles)
#' @param data_experiment Data of the experiment (matrix of ints)
#'
#' @return The log-likelihood of the theory
#'
#' @examples
log_lh <- function(model_prediction, data_experiment) {
  # data_experiment: 2x2, integers
  # model_prediction: 2x2, double, rowSum = 1, single model!

  # input consistency checks
  # input -> output
  # TODO: Fix!
  output <- rowSums(data_experiment * log(model_prediction)) # not correct, need to add all?
  # output <- data_experiment * log(model_prediction)+(rowSums(data_experiment)-data_experiment)*log(1-model_prediction) old 2x2
  # output consistency checks
  return(output)
}
