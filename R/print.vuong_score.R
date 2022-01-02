
# TODO: Fix this one
# TODO: Add readme and stuff
print.vuong_score<-function(obj){
  cat("Comparing", obj$theory_1, "to", obj$theory_2, "yields a Vuong Score of", obj$score, ".\n")
  if(obj$score > 2) {cat("This means that", obj$theory_2, "is better.")}
  if(obj$score < 2) {cat("This means that", obj$theory_1, "is better.")}
}
