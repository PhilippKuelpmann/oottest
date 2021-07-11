# To rewrite
# log_lh
# vuong_denominator
# vuong_z_score



# To add:
# chi Square test

nullTesting <- function(game){
  matrixOfResults <- matrix(rep(getPredictions(game)[1,],each=nrow(getPredictions(game)[c(-1,-2,-3),])),nrow=nrow(getPredictions(game)[c(-1,-2,-3),]))
  zeds <- 147*(matrixOfResults-getPredictions(game)[c(-1,-2,-3),])^2/(getPredictions(game)[c(-1,-2,-3),]*(1-getPredictions(game)[c(-1,-2,-3),]))
  chiSquare <- rowSums(zeds)
  pValue <- pchisq(rowSums(zeds),df=10,lower.tail=FALSE)
  rbind(chiSquare, pValue)
}

nullTestingOutput <- function(name){
  output <- t(rbind(nullTesting(HDG),nullTesting(MP)))
  colnames(output) <- c("chi-square(HDG)","p-value(HDG)", "chi-square(MP)","p-value(MP)")
  output[,c(2,4)] <- "<0.00001"
  output[,c(1,3)] <- round(as.numeric(output[,c(1,3)]), digits = 2)
  output <- as.data.frame(output)
  filename <- paste("tables/",name,".tex",sep="")
  print(xtable(output, type = "latex"), file = filename, only.content = TRUE)
}


# output? Table/graph?
