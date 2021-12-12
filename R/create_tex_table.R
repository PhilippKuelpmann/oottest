# Create tex table, colored and stuff.


coloredVuongTable  <- function(vuong_matrix){
  makeGreen <- function(x){paste0(' \\cellcolor{green!50} ', x)}
  makeLessGreen <- function(x){paste0(' \\cellcolor{green!25} ', x)}
  makeRed <- function(x){paste0(' \\cellcolor{darkred} \\color{white} ', x)}
  makeLessRed <- function(x){paste0(' \\cellcolor{darkred!80} \\color{white} ', x)}
  mat <- round(vuong_matrix,digit=2)
  mat <- ifelse(mat < -2,makeRed(mat),
                ifelse(mat > 2,makeGreen(mat),
                       ifelse(mat < -1, makeLessRed(mat),
                              ifelse(mat > 1, makeLessGreen(mat),mat))))
  mat <- xtable(mat, type = "latex")
  print(mat, sanitize.text.function = identity)
}
