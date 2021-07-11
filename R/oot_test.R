oot_test <- function(data_frequencies, model_predictions) {
  ## Input consistency checks
  # TODO: replace data_frequencies with actual data (or frequencies + num_subjects)

  # TODO: Add outputs
  stopifnot(!missing(data_frequencies), !missing(model_predictions))
  stopifnot(class(data_frequencies)=="matrix")
  stopifnot(typeof(data_frequencies)=="double")

  stopifnot(class(model_predictions)=="array")
  stopifnot(typeof(model_predictions)=="double")
  # model_predictions needs to have 3 dimensions: Treatment x Predictions x Theories
  stopifnot(length(dim(model_predictions))==3)
  # model_predictions needs to have 2 dimensions: Treatment x Predictions
  stopifnot(length(dim(model_predictions)) == 2)

  stopifnot(dim(model_predictions)[1:2] == dim(data_frequencies))
  stopifnot(dim(model_predictions)[3] > 1) # can't compare just 1 theory - only warning?

  # Test if all theories and data sum up to 1 for each treatment: difference smaller than .Machine$double.eps
  # Should I make this a warning?
  stopifnot(abs(colSums(model_predictions,dims = 1)-1) < .Machine$double.eps)
  stopifnot(abs(colSums(data_frequencies,dims = 1)-1) < .Machine$double.eps)


  # Function
  # Output consistency checks
  return(output)
}
