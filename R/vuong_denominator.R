# Own implementation of the Vuong test for our models (no free parameter, binomial)

vuong_denominator <- function(model1_prediction, model2_prediction, experiment_data){
  predI <- getPredictions(game)[predictionFirst+3,] # +3 to ignore Data, x and y
  predJ <- getPredictions(game)[predictionBase+3,]
  probData <- getPredictions(game)[1,]
  num_subjects <- rowSums(experiment_data)
  # sum_{all t} n * p * (1-p) [log(p_i/pj) - log((1-p_i)/(1-p_j))]
  output <- experiment_data*(1-experiment_data/num_subjects)*(log(model1_prediction/model2_prediction)-log((1-model1_prediction)/(1-model2_prediction)))^2 # where is the ^2 coming from?
  return(sum(output))
}
