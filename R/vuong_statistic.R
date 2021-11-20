#' Getting the log-likelihood ratio
#'
#' @noRd
get_llr <- function(data, pred_I, pred_J) {
  # likelihood ratio of theory i and j
  result <- data * log(pred_I / pred_J)
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
#' pred[action, treatment]
#' Only for 3 categories and broken right now
#'
#' @noRd
get_variance_of_llr_3 <- function(data, pred_I, pred_J) {
  treatments <- ncol(data)
  n <- colSums(data)
  output <- rep(0, treatments)
  for (t in 1:treatments) {
    output[t] <- output[t] + sum(data[, t] * log(pred_I[, t] / pred_J[, t])^2) # summing over all actions
    for (l in 1:2) {
      for (m in (l+1):3) {
        output[t] <- output[t] - 2 * (data[l, t] * data[m, t]) / n[t]  * log(pred_I[l, t]/pred_J[l, t]) * log(pred_I[m, t]/pred_J[m, t])
      }
    }
  }
  return(sum(output))
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


#' Implementation of the Vuong test for our setting 3x3
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param pred_I Model prediction for a theory (matrix of doubles/data.frame): columns are prediction of playing action 1,2,3..., rows are treatments
#' @param pred_J Model prediction for a theory (matrix of doubles/data.frame): columns are prediction of playing action 1,2,3..., rows are treatments
#'
#' @return The z score of testing theory I against theory J, given the data
#'
#' @examples (missing)
#' @export
vuong_statistic_3 <- function(data, pred_I, pred_J) {
  result <- get_llr(data, pred_I, pred_J) / get_variance_of_llr_3(data, pred_I, pred_J)^(.5)
  return(result)
}

#' Getting a vuong matrix
#'
#' @param data Data of the experiment (matrix of ints/data.frame): columns are observations of action 1,2,3..., rows are treatments.
#' @param theories Model predictions for all theories. (how to structure? List of matrices?)
#'
#' @return Table of z-scores for all comparisons
#'
#' @examples (missing)
#' @export
vuong_matrix_3 <- function(data, theories) {
  num_theories <- length(theories)
  result <- matrix(, nrow=num_theories, ncol=num_theories)
  for (i in 1:num_theories) {
    for (j in 1:num_theories) {
      result[i,j] <- vuong_statistic_3(data, theories[[i]], theories[[j]])
    }
  }
  return(result)
}

# Random testing stuff
# TODO: Move

# AC = HDG, RSP = MP
# num_theories <- length(all_theories_3)
# theories_ac <- list()
# theories_rsp <- list()
# short <- c()
# for (theory in 1:num_theories) {
#   theories_ac[[theory]] <- t(all_theories_3[[theory]]@predictions_HDG)
#   theories_rsp[[theory]] <- t(all_theories_3[[theory]]@predictions_RSP)
#   short[[theory]] <- all_theories_3[[theory]]@short
# }

# ac_results <- vuong_matrix_3(ac_data, theories = theories_ac)
# rsp_results <- vuong_matrix_3(rsp_data, theories = theories_rsp)
# rownames(ac_results) <- short
# rownames(rsp_results) <- short
# colnames(ac_results) <- short
# colnames(rsp_results) <- short

# library("openxlsx")
# write.xlsx(format(as.data.frame(ac_results), digits=2), "vuong_scores_ac.xlsx", asTable=TRUE, colNames=TRUE, rowNames=TRUE)
# write.xlsx(format(as.data.frame(rsp_results), digits=2), "vuong_scores_rsp.xlsx", asTable=TRUE, colNames=TRUE, rowNames=TRUE)



# t(all_theories_3[[1]]@predictions_HDG)
# test_data <-matrix(c(2,2,2,1,2,3), nrow=3, ncol=5)
# theory_1 <- matrix(c(1/3,1/3,1/3), nrow=3, ncol=2)
# theory_2 <- matrix(c(1/4,1/4,1/2), nrow=3, ncol=2)
# vuong_statistic_3(test_data, theory_1, theory_2)
# ExperimentalData <- matrix(data = 2, nrow = 4, ncol = 2, byrow=TRUE)
# PredictionI <- matrix(data = c(.5, .5, .6, .4, .7, .3, .8, .2), nrow = 4, ncol = 2, byrow=TRUE)
# PredictionJ <- matrix(data = c(.5, .5, .5, .5, .5, .5, .5, .5), nrow = 4, ncol = 2, byrow=TRUE)
