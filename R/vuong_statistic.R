#' Implementation of the Vuong test for our setting
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are treatments, rows are actions, values are number of subjects who chose each action on each treatment
#' @param pred_i Model prediction for a theory (matrix of doubles/data.frame): rows are prediction of playing action 1,2,3..., columns are treatments
#' @param pred_j Model prediction for a theory (matrix of doubles/data.frame): rows are prediction of playing action 1,2,3..., columns are treatments
#'
#' @return The z score of testing theory I against theory J, given the data
#'
#' @examples vuong_statistic(data_two_action_games, predictions_two_action_games[,,1], predictions_two_action_games[,,2])
#' @export
vuong_statistic <- function(data, pred_i, pred_j) {
  # number of treatments and of actions are the same in data, pred_i and pred_j
  stopifnot(
    all.equal(dim(data), dim(pred_i), check.names = FALSE, check.attributes = FALSE),
    all.equal(dim(data), dim(pred_j), check.names = FALSE, check.attributes = FALSE)
  )

  # the sum of all predictions for each treatment sum up to one
  for (treatment in seq_len(ncol(pred_i))) {
    stopifnot(
      all.equal(colSums(pred_i)[treatment], 1, check.names = FALSE, check.attributes = FALSE),
      all.equal(colSums(pred_j)[treatment], 1, check.names = FALSE, check.attributes = FALSE)
    )
  }

  for (col in data) {
    for (i in col) {
      if (i %% 1 != 0) stop("Data not integers")
    }
  }

  result <- get_llr(data, pred_i, pred_j) / get_variance_of_llr(data, pred_i, pred_j)^(.5)
  return(result)
}


#' Getting a vuong matrix
#'
#' @param data matrix rows: choices, columns: treatments
#' @param theories list of matrices each in the same format as above
#'
#' @return Table of z-scores for all comparisons
#'
#' @examples vuong_matrix(data_two_action_games, predictions_two_action_games)
#' @export
vuong_matrix <- function(data, theories) {
  for (col in data) {
    for (i in col) {
      if (i %% 1 != 0) stop("Data not integers")
    }
  }
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
