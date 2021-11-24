# Create tex table, colored and stuff.

# vuongTable <- function(game){
#   numTheories <- nrow(getLHs(game))
#   mat <- matrix(nrow=numTheories,ncol=numTheories)
#   for (i in 1:numTheories) {
#     for (j in 1:numTheories) {
#       mat[j,i] <- vuong(i,j,game)
#     }
#   }
#   mat[mat=="NaN"]<-0
#   names <- rownames(getLHs(HDG))
#   colnames(mat) <- names
#   rownames(mat) <- names
#   mat
# }
#
# coloredVuongTable  <- function(){
# makeRed <- function(x){paste0('\\cellcolor{red!50} ', x)}
#   makeGreen <- function(x){paste0(' \\cellcolor{green!50} ', x)}
# makeLessRed <- function(x){paste0('\\cellcolor{red!25} ', x)}
#   makeLessGreen <- function(x){paste0(' \\cellcolor{green!25} ', x)}
#   makeRed <- function(x){paste0(' \\cellcolor{darkred} \\color{white} ', x)}
#   makeLessRed <- function(x){paste0(' \\cellcolor{darkred!80} \\color{white} ', x)}
#   mat <- round(mat,digit=2)
#   #mat[mat > 2] <- paste("cellcolor{red!25}", mat[mat > 2])
#   #mat[mat < - 2] <- paste("cellcolor{green!25}", mat[mat < - 2])
#   mat <- ifelse(mat > 2,makeRed(mat),
#                 ifelse(mat < -2,makeGreen(mat),
#                        ifelse(mat > 1, makeLessRed(mat),
#                               ifelse(mat < -1, makeLessGreen(mat),mat))))
#   mat <- xtable(mat, type = "latex")
#   print(mat, sanitize.text.function = identity)
# }
