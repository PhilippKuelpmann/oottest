#' Color a Vuong matrix in red/green
#'
#' requires the xcolor package and \\definecolor\{darkred\}\{rgb\}\{0.55, 0.0, 0.0\}
#' Red/green, but still kind of readable in black/white
#'
#' @param vuong_table a Voung table like the output of vuong_matrix
#'
#' @return Latex code of a colored vuong table
#'
#' @noRd
color_vuong_table <- function(vuong_table) {
  # TODO: replace darkred with {rgb}{0.55, 0.0, 0.0}?

  vuong_table[is.na(vuong_table)] <- 0
  make_green <- function(x) {
    paste0(" \\cellcolor{green!50} ", x)
  }
  make_less_green <- function(x) {
    paste0(" \\cellcolor{green!25} ", x)
  }
  make_red <- function(x) {
    paste0(" \\cellcolor{darkred} \\color{white} ", x)
  }
  make_less_red <- function(x) {
    paste0(" \\cellcolor{darkred!80} \\color{white} ", x)
  }
  mat <- round(vuong_table, digits = 2)
  mat <- ifelse(mat < -2, make_red(mat),
    ifelse(mat > 2, make_green(mat),
      ifelse(mat < -1, make_less_red(mat),
        ifelse(mat > 1, make_less_green(mat), mat)
      )
    )
  )
  mat <- xtable::xtable(mat, type = "latex")
  print(mat, sanitize.text.function = identity)
}
