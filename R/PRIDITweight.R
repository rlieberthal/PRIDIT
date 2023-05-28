PRIDITweight <- function(riditscores) { # riditscores should have ID in the first column
  Bijmatrix <- data.matrix(riditscores[,2:ncol(riditscores)])
  Bijtrans <- t(Bijmatrix)
  Bijsq <- Bijtrans %*% Bijmatrix
  Bijss <- diag(Bijsq)
  Bijsum <- sqrt(Bijss)
  summat <- t(matrix(Bijsum,ncol(Bijmatrix),nrow(Bijmatrix)))
  Bijnorm <- Bijmatrix/summat
  pc <- princomp(Bijmatrix, cor=TRUE)	
  maxeigval <- (pc$sdev[1])^2
  maxeigvec <- pc$load[,1]
  weightvec <- maxeigvec*pc$sdev[1]
  #	weightvec <- weightvec * -1		# PC problem - sign switch
  weightvec
}