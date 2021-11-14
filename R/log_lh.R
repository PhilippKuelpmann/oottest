#' Calculate log-likelihood for a theory
#'
#' @param model_prediction Model prediction for a theory (matrix of doubles/data.frame): columns are prediction of playing action 1,2,3..., rows are treatments
#' @param data_experiment Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments
#'
#' @return The log-likelihood of the theory for each treatment
#'
#' @examples
#' @export
log_lh <- function(model_prediction, data_experiment) {
  # data_experiment: 2x2, integers
  # test_data <- matrix(data = 1:3, nrow = 3, ncol = 3, byrow=TRUE)
  # model_prediction: 2x2, double, rowSum = 1, single model!
  # test_pred <- matrix(data = c(.3, .3, .3, .3, .3, .3, .4, .4, .4), nrow = 3, ncol = 3)
  # test_pred <- matrix(data = c(.2, .2, .2, .3, .3, .3, .5, .5, .5), nrow = 3, ncol = 3)


  if (!is.numeric(model_prediction)) {
    stop("The model prediction must be numberic.")
  }
  if (!is.numeric(data_experiment)) {
    stop("The model prediction must be numberic.")
  }

  if (nrow(model_prediction) != nrow(data_experiment)) {
    stop("Predictions and data must have the same number of actions.")
  }

  if (ncol(model_prediction) != ncol(data_experiment)) {
    stop("Predictions and data must have the same number of treatments.")
  }

  # input consistency checks
  # input -> output
  output <- rowSums(data_experiment * log(model_prediction))
  # output consistency checks
  return(output)
}
