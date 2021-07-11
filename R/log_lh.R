log_lh <- function(model_prediction, data_experiment) {
  # data_experiment: 2x2, integers
  # model_prediction: 2x2, double, rowSum = 1, single model!
  # input consistency checks
  # input -> output
  output <- data_experiment * log(model_prediction)+(rowSums(data_experiment)-data_experiment)*log(1-model_prediction)
  # output consistency checks
  return(output)
}
