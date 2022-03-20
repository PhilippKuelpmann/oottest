x <- vuong_matrix(data_two_action_games, predictions_two_action_games)
results <- c()
for ( i in (1: dim(predictions_two_action_games)[3])){
  results <- append(results, vuong_statistic(data_two_action_games, predictions_two_action_games[,,i], predictions_two_action_games[,,i]))
}

expect_equal(results, rep(NaN,  dim(predictions_two_action_games)[3]))
