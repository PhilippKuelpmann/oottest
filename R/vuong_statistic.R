#' Implementation of the Vuong test for our setting
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments. matrix rows: choices, columns: treatments
#' @param pred_I Model prediction for a theory (matrix of doubles/data.frame): rows are prediction of playing action 1,2,3..., columns are treatments
#' @param pred_J Model prediction for a theory (matrix of doubles/data.frame): rows are prediction of playing action 1,2,3..., columns are treatments
#'
#' @return The z score of testing theory I against theory J, given the data
#'
#' @examples (missing)
#' @export
vuong_statistic <- function(data, pred_I, pred_J) {
  if( typeof(data) != "integer") stop("Data should be type integer")
  if( colSums(pred_I) < 0 |colSums(pred_I) > 1)  stop('Prediction sum not between 0 and 1')
  if( colSums(pred_J) < 0 |colSums(pred_J) > 1)  stop('Prediction sum not between 0 and 1')
  # data: all ints
  # if colSum of predictions is 1 and each is between 0,1
  # check if rows and columns are of the same length

  result <- get_llr(data, pred_I, pred_J) / get_variance_of_llr(data, pred_I, pred_J)^(.5)
  return(result)
}


#' Getting a vuong matrix
#'
#' @param data matrix rows: choices, columns: treatments
#' @param theories list of matrices each in the same format as above
#'
#' @return Table of z-scores for all comparisons
#'
#' @examples (missing)
#' @export
vuong_matrix <- function(data, theories) {
  # data: all ints, length(data rows) = length(theory rows), ...
  # theories: sum of each treatment for each theory = 1
  # in both theories and data: same number of actions (p1/p2/p3 or A/B/C) and same number of treatments (T01-T40)
  num_theories <- dim(theories)[3]
  result <- matrix(, nrow = num_theories, ncol = num_theories)
  for (i in 1:num_theories) {
    for (j in 1:num_theories) {
      result[i, j] <- vuong_statistic(data, theories[, , i], theories[, , j])
    }
  }
  colnames(result) <- colnames(theories[1, , ])
  rownames(result) <- colnames(theories[1, , ])
  return(result)
}
