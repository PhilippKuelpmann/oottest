#' Color a Vuong matrix in red/green
#' requires the xcolor package and \definecolor{darkred}{rgb}{0.55, 0.0, 0.0}
#' Red/green, but still kind of readable in black/white
#'
#' @param vuong_table a Voung table like the output of vuong_matrix
#'
#' @return Latex code of a colored vuong table
#'
#' @examples (missing)
#' @export


color_vuong_table <- function(vuong_table) {
  # TODO: replace darkred with {rgb}{0.55, 0.0, 0.0}?
  # TODO: require xtable
  makeGreen <- function(x) {
    paste0(" \\cellcolor{green!50} ", x)
  }
  makeLessGreen <- function(x) {
    paste0(" \\cellcolor{green!25} ", x)
  }
  makeRed <- function(x) {
    paste0(" \\cellcolor{darkred} \\color{white} ", x)
  }
  makeLessRed <- function(x) {
    paste0(" \\cellcolor{darkred!80} \\color{white} ", x)
  }
  mat <- round(vuong_matrix, digit = 2)
  mat <- ifelse(mat < -2, makeRed(mat),
    ifelse(mat > 2, makeGreen(mat),
      ifelse(mat < -1, makeLessRed(mat),
        ifelse(mat > 1, makeLessGreen(mat), mat)
      )
    )
  )
  mat <- xtable(mat, type = "latex")
  print(mat, sanitize.text.function = identity)
}
