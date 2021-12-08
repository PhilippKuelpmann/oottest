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
    theories_rsp_p1[[theory]] <- t(all_theories_3[[theory]]@predictions_RSP[1:5, ])
    theories_rsp_p2[[theory]] <- t(all_theories_3[[theory]]@predictions_RSP[6:10, ])
    short[[theory]] <- all_theories_3[[theory]]@short
  }
  ac_results <- vuong_matrix(ac_data, theories = theories_ac)
  rsp_results <- vuong_matrix(rsp_data, theories = theories_rsp)
  rsp_results_p1 <- vuong_matrix(rsp_data[, 1:5], theories = theories_rsp_p1)
  rsp_results_p2 <- vuong_matrix(rsp_data[, 6:10], theories = theories_rsp_p2)
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



#' Reproduce data from Külpmann Kuzmics: 2 action, 2 player games, chi-square tests
#'
#' @noRd
reproduce_2_chi_sq_mp <- function() {
  output <- c()
  num_theories <- ncol(all_theories_2)
  for (i in 1:num_theories) {
    pred_MP <- rbind(all_theories_2[,i]$MPpredictions, 1- all_theories_2[,i]$MPpredictions)
    # pred_HDG <- rbind(all_theories_2[,theory]$HDGpredictions, 1- all_theories_2[,theory]$HDGpredictions)
    output <- rbind(output, get_chi_sq(mp_data, pred_MP))
  }
  return(output)
}

#' Reproduce data from Külpmann Kuzmics: 2 action, 2 player games, chi-square tests
#'
#' @noRd
reproduce_2_chi_sq_hdg <- function() {
  output <- c()
  num_theories <- ncol(all_theories_2)
  for (i in 1:num_theories) {
    pred <- rbind(all_theories_2[,i]$HDGpredictions, 1- all_theories_2[,i]$HDGpredictions)
    output <- rbind(output, get_chi_sq(hdg_data, pred))
  }
  return(output)
}

#' Reproduce data from Külpmann Kuzmics: 3 action, 2 player games, chi-square tests
#'
#' @noRd
reproduce_3_chi_sq_hdg <- function() {
  output <- c()
  num_theories <- length(all_theories_3)
  for (i in 1:num_theories) {
    pred <- t(all_theories_3[[i]]@predictions_HDG)
    # pred_HDG <- rbind(all_theories_2[,theory]$HDGpredictions, 1- all_theories_2[,theory]$HDGpredictions)
    output <- rbind(output, get_chi_sq(ac_data, pred))
  }
  return(output)
}


#' Reproduce data from Külpmann Kuzmics: 3 action, 2 player games, chi-square tests
#'
#' @noRd
reproduce_3_chi_sq_ac <- function() {
  output <- c()
  num_theories <- length(all_theories_3)
  for (i in 1:num_theories) {
    pred <- t(all_theories_3[[i]]@predictions_RSP)
    # pred_HDG <- rbind(all_theories_2[,theory]$HDGpredictions, 1- all_theories_2[,theory]$HDGpredictions)
    output <- rbind(output, get_chi_sq(rsp_data, pred))
  }
  return(output)
}


#' Reproduce data from Külpmann Kuzmics: likelihood tables, second part
#'
#' @noRd
create_likelihood_tex_table_3 <- function(game = "AC") {
  #AC or RSP
  output_table <- c()
  names <- c()
  if (game == "RSP") {
    for (i in 1:14) {
      output_table <- rbind(output_table, get_log_lh(rsp_data, prediction = t(all_theories_3[[i]]@predictions_RSP)))
      names <- c(names, all_theories_3[[i]]@short)
    }
  } else {
    for (i in 1:14) {
      output_table <- rbind(output_table, get_log_lh(ac_data, prediction = t(all_theories_3[[i]]@predictions_HDG)))
      names <- c(names, all_theories_3[[i]]@short)
    }
  }
  rownames(output_table) <- names
  if (game == "RSP") {
    output_table_asym <- cbind(output_table[,1:5], rowSums(output_table[,1:5]))
    output_table_sym <- cbind(output_table[,6:10], rowSums(output_table[,6:10]))
    colnames(output_table_asym) <- c("T1", "T2", "T3", "T4","T5", "SUM")
    colnames(output_table_sym) <- c("T6","T7", "T8","T9", "T10", "SUM")
    print(xtable(output_table_asym, type = "latex"))
    print(xtable(output_table_sym, type = "latex"))
  }
  output_table <- cbind(output_table, rowSums(output_table))
  colnames(output_table) <- c("T1", "T2", "T3", "T4","T5", "T6","T7", "T8","T9", "T10", "SUM")
  print(xtable(output_table, type = "latex"))
}
