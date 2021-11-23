#' Reproduce data from Külpmann Kuzmics: 3 action, 2 player games
#'
#' @noRd
reproduce_3_actions <- function() {
    num_theories <- length(all_theories_3)
    theories_ac <- list()
    theories_rsp <- list()
    theories_rsp_p1 <- list()
    theories_rsp_p2 <- list()
    short <- c()
    for (theory in 1:num_theories) {
     theories_ac[[theory]] <- t(all_theories_3[[theory]]@predictions_HDG)
     theories_rsp[[theory]] <- t(all_theories_3[[theory]]@predictions_RSP)
     theories_rsp_p1[[theory]] <- t(all_theories_3[[theory]]@predictions_RSP[1:5,])
     theories_rsp_p2[[theory]] <- t(all_theories_3[[theory]]@predictions_RSP[6:10,])
     short[[theory]] <- all_theories_3[[theory]]@short
    }
    ac_results <- vuong_matrix(ac_data, theories = theories_ac)
    rsp_results <- vuong_matrix(rsp_data, theories = theories_rsp)
    rsp_results_p1 <- vuong_matrix(rsp_data[,1:5], theories = theories_rsp_p1)
    rsp_results_p2 <- vuong_matrix(rsp_data[,6:10], theories = theories_rsp_p2)
    rownames(ac_results) <- short
    rownames(rsp_results) <- short
    colnames(ac_results) <- short
    colnames(rsp_results) <- short
    rownames(rsp_results_p1) <- short
    rownames(rsp_results_p2) <- short
    colnames(rsp_results_p1) <- short
    colnames(rsp_results_p2) <- short
    return(list(ac_results, rsp_results, rsp_results_p1, rsp_results_p2))
}

#library("openxlsx")
#write.xlsx(format(as.data.frame(rsp_results_p1), digits=2), "vuong_scores_rsp_p1.xlsx", asTable=TRUE, colNames=TRUE, rowNames=TRUE)
#write.xlsx(format(as.data.frame(rsp_results_p2), digits=2), "vuong_scores_rsp_p2.xlsx", asTable=TRUE, colNames=TRUE, rowNames=TRUE)


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
