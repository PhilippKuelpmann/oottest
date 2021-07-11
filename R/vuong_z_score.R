# vuong_z_score: lh_ratio/vuong_denominator^(1/2):
vuong_z_score <- function(model1_prediction, model2_prediction, experiment_data) {
  output <- (log_lh(model1_prediction, experiment_data) - log_lh(model2_prediction, experiment_data))/vuong_denominator^(.5) # ^(1/(length of strategy vector?))
  return(output)
}
