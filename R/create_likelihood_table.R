#' Getting a likelihood table
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param predictions Model prediction for one theory: rows/cols as data
#'
#' @return Table of z-scores for all comparisons
#' @noRd
create_likelihood_table <- function(data, predictions) {
  output_table <- c()
  for (i in 1:14) {
    output_table <- rbind(output_table, get_log_lh(data, prediction = predictions[, , i]))
  }
  colnames(output_table) <- names(predictions[1, , 1])
  rownames(output_table) <- names(predictions[1, 1, ])
  return(output_table)
}
