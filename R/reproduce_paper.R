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


#' Reproduce data from Külpmann Kuzmics: likelihood tables, first part
#'
#' @noRd
create_likelihood_tex_tables_2 <- function() {
  output_table <- c()
  pred_2 <- clean_theory_predictions_2()
  data_2 <- cbind(hdg_data, mp_data)
  for (i in 1:14) {
    output_table <- rbind(output_table, get_log_lh(data_2, prediction = pred_2[[i]]))
  }
  rownames(output_table) <- names(pred_2)
  colnames(output_table) <- c("T01", "T02", "T03", "T04","T05", "T06","T07", "T08","T09", "T10", "T11", "T12", "T13", "T14","T15", "T16","T17", "T18","T19", "T20")
  return(output_table)
}

#' Reproduce data from Külpmann Kuzmics: likelihood tables, second part
#'
#' @noRd
create_likelihood_tex_tables_3 <- function(game = "AC") {
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
  output_table <- cbind(output_table, rowSums(output_table))
  if (game == "RSP") {
    output_table_asym <- cbind(output_table[,1:5], rowSums(output_table[,1:5]))
    output_table_sym <- cbind(output_table[,6:10], rowSums(output_table[,6:10]))
    colnames(output_table) <- c("T31", "T32", "T33", "T34","T35", "T36","T37", "T38","T39", "T40", "SUM")
    colnames(output_table_asym) <- c("T31", "T32", "T33", "T34","T35", "SUM")
    colnames(output_table_sym) <- c("T36","T37", "T38","T39", "T40", "SUM")
    print(xtable(output_table_asym, type = "latex"))
    print(xtable(output_table_sym, type = "latex"))
  } else {
    colnames(output_table) <- c("T21", "T22", "T23", "T24","T25", "T26","T27", "T28","T29", "T30", "SUM")
  }
  return(output_table)
}


#' clean predictions for 2x2 games
#'
#' @noRd
clean_theory_predictions_2 <- function(add_third = FALSE) {
  predictions <- list()
  theory_names <- c()
  for (theory in 1:14) {
    hdg_pred <- all_theories_2[,theory]$HDGpredictions
    mp_pred <- all_theories_2[,theory]$MPpredictions
    predictions[[theory]] <- rbind(c(hdg_pred, mp_pred), c(1-hdg_pred, 1-mp_pred))
    row.names(predictions[[theory]]) <- c("A", "B")
    if (add_third == TRUE) {
      predictions[[theory]] <- rbind(predictions[[theory]], 0)
    }
    colnames(predictions[[theory]]) <- c("T01", "T02", "T03", "T04","T05", "T06","T07", "T08","T09", "T10", "T11", "T12", "T13", "T14","T15", "T16","T17", "T18","T19", "T20")
    theory_names <- c(theory_names, all_theories_2[,theory]$short)
  }
  names(predictions) <- theory_names
  return(predictions)
}

#' clean predictions for 3x2 games
#'
#' @noRd
clean_theory_predictions_3 <- function() {
  num_theories <- length(all_theories_3)
  theories_ac <- list()
  theories_rsp <- list()
  predictions <- list()
  theory_names <- c()
  for (theory in 1:num_theories) {
    predictions[[theory]] <- t(rbind(all_theories_3[[theory]]@predictions_HDG, all_theories_3[[theory]]@predictions_RSP))
    colnames(predictions[[theory]]) <- c("T21", "T22", "T23", "T24","T25", "T26","T27", "T28","T29", "T30", "T31", "T32", "T33", "T34","T35", "T36","T37", "T38","T39", "T40")
    theory_names <- c(theory_names, all_theories_3[[theory]]@short)
  }
  names(predictions) <- theory_names
  return(predictions)
}


#' Reproduce data from Külpmann Kuzmics: Vuong table: 2x2
#'
#' @noRd
create_vuong_table_2 <- function() {
  # merge data
  data_2 <- cbind(hdg_data, mp_data)
  preds_2 <- clean_theory_predictions_2()
  result <- vuong_matrix(data_2, theories = preds_2)
  colnames(result) <- names(preds_2)
  row.names(result) <- names(preds_2)
  return(result)
}

#' Reproduce data from Külpmann Kuzmics: Vuong table: 3x2
#'
#' @noRd
create_vuong_table_3 <- function() {
  # merge data
  data_3 <- cbind(ac_data, rsp_data)
  preds_3 <- clean_theory_predictions_3()
  result <- vuong_matrix(data_3, theories = preds_3)
  colnames(result) <- names(preds_3)
  row.names(result) <- names(preds_3)
  return(result)
}

#' Reproduce data from Külpmann Kuzmics: Vuong table: all
#'
#' (llr_2 + llr_3)/((variance_2 + variance_3))^(.5)
#' Need to adjust the variance and llr by hand
#' @noRd
create_vuong_table_all <- function() {
  # (llr_2 + llr_3)/((variance_2 + variance_3))^(.5)
  data_2 <- cbind(hdg_data, mp_data)
  data_3 <- cbind(ac_data, rsp_data)
  preds_2 <- clean_theory_predictions_2()
  preds_3 <- clean_theory_predictions_3()
  names <- c()
  num_theories <- 14
  result <- matrix(, nrow = num_theories, ncol = num_theories)
  for (i in 1:num_theories) {
    for (j in 1:num_theories) {
      variance_2 <- get_variance_of_llr(data_2, preds_2[[i]], preds_2[[j]])
      variance_3 <- get_variance_of_llr(data_3, preds_3[[i]], preds_3[[j]])
      llr_2 <- get_llr(data_2, preds_2[[i]], preds_2[[j]])
      llr_3 <- get_llr(data_3, preds_3[[i]], preds_3[[j]])
      result[i, j] <- (llr_2 + llr_3)/(variance_2 + variance_3)^(.5)
    }
  }
  colnames(result) <- names(preds_3)
  row.names(result) <- names(preds_3)
  return(result)
}

# >>>>> Old stuff <<<<<
# c("T01", "T02", "T03", "T04","T05", "T06","T07", "T08","T09", "T10", "T11", "T12", "T13", "T14","T15", "T16","T17", "T18","T19", "T20")
# c("T21", "T22", "T23", "T24","T25", "T26","T27", "T28","T29", "T30", "T31", "T32", "T33", "T34","T35", "T36","T37", "T38","T39", "T40")
# colnames(ac_data) <- c("T21", "T22", "T23", "T24","T25", "T26","T27", "T28","T29", "T30")
# colnames(rsp_data) <- c("T31", "T32", "T33", "T34","T35", "T36","T37", "T38","T39", "T40")
# row.names(rsp_data) <- c("A", "B", "C")

# library("xtable")
# xtable(create_vuong_table_2(), format="latex")
# xtable(create_vuong_table_3(), format="latex")
# xtable(create_vuong_table_all(), format="latex")

#' Reproduce data from Külpmann Kuzmics: LLR graph
#'
#' Need to adjust the variance and llr by hand
#' @noRd
create_llr_graph_files <- function(){
  create_likelihood_tex_tables_3(game="AC")[,-11]
  create_likelihood_tex_tables_3(game="RSP")[,-11]
  llr_3 <- cbind(create_likelihood_tex_tables_3(game="AC")[,-11], create_likelihood_tex_tables_3(game="RSP")[,-11])
  llr_2 <- create_likelihood_tex_tables_2()
  llr <- cbind(llr_2, llr_3)
  # 2: NA-RA
  # 5: CH_RA <- normalized to 1
  # 9: QLK-RA
  # 11: QCH-RA

  CH_normalizer <- llr[5,]
  NE_RA <- round(t(rbind(1:40, llr[2,]-CH_normalizer)),12)
  CH_RA <- round(t(rbind(1:40, llr[5,]-CH_normalizer)),12)
  QLK_RA <- round(t(rbind(1:40, llr[9,]-CH_normalizer)),12)
  QCH_RA <- round(t(rbind(1:40, llr[11,]-CH_normalizer)),12)
  colnames(NE_RA) <- c("treatment", "lh_diff")
  colnames(CH_RA) <- c("treatment", "lh_diff")
  colnames(QLK_RA) <- c("treatment", "lh_diff")
  colnames(QCH_RA) <- c("treatment", "lh_diff")
  write.csv(NE_RA, file="NE_RA.csv", row.names = FALSE, quote = FALSE)
  write.csv(CH_RA, file="CH_RA.csv", row.names = FALSE, quote = FALSE)
  write.csv(QLK_RA, file="QLK_RA.csv", row.names = FALSE, quote = FALSE)
  write.csv(QCH_RA, file="QCH_RA.csv", row.names = FALSE, quote = FALSE)
}

