PRIDITscore <- function(riditscores,IDvector,weightvec)	{ # riditscores should have ID in the first column
  Bijmatrix <- data.matrix(riditscores[,2:ncol(riditscores)])
  Bijtrans <- t(Bijmatrix)
  Bijsq <- Bijtrans %*% Bijmatrix
  Bijss <- diag(Bijsq)
  Bijsum <- sqrt(Bijss)
  summat <- t(matrix(Bijsum,ncol(Bijmatrix),nrow(Bijmatrix)))
  weightmat <- t(matrix(weightvec,ncol(Bijmatrix),nrow(Bijmatrix)))
  Bijnorm <- Bijmatrix/summat
  pc <- princomp(Bijmatrix, cor=TRUE)	
  maxeigval <- (pc$sdev[1])^2
  scoremat <- (weightmat*Bijnorm)/maxeigval
  templ <- matrix(1,ncol(Bijmatrix),1)
  scorevec <- scoremat %*% templ
  results.mat <- matrix(0,nrow(Bijmatrix),2)
  results.mat[,1] <- IDvector
  results.mat[,2] <- scorevec
  # results.mat <- matrix(0,nrow(Bijmatrix),2)
  # results.mat[,1] <- IDvector
  # results.mat[,2] <- scorevec
  # write.table(results.mat, file = "results.csv", sep = ",", col.names = NA)
  # write.table(weightvec, file = "weight.csv", sep = ",", col.names = NA)
  results <- data.frame(Claim.ID=IDvector,PRIDITscore=scorevec)
}