ridit <- function(allrawdata) { # allrawdata should have ID in the first column
  IDvector <- allrawdata[,1]
  rawdata <- data.matrix(allrawdata[,2:ncol(allrawdata)])
  Fmat <- matrix(0,nrow(rawdata),ncol(rawdata))
  Fmatmin <- matrix(0,nrow(rawdata),ncol(rawdata))
  Fmatplu <- matrix(0,nrow(rawdata),ncol(rawdata))
  bmat <- matrix(0,nrow(rawdata),ncol(rawdata))
  for(i in 1:ncol(rawdata))
  {Fn <- ecdf(rawdata[,i]) 
  Fmatplu[,i] <- 1-Fn(rawdata[,i])
  Fmatmin[,i] <- Fn(rawdata[,i]-0.001) # Make 0.001 the smallest possible incrememnt!
  }
  Bij <- Fmatmin[,1:ncol(rawdata)]-Fmatplu[,1:ncol(rawdata)]
  for(j in 1:ncol(Bij))
  {Bij.df <- data.frame(Bij[,j])
  Bij.vec <- data.matrix(Bij.df)
  Bij.vec[is.na(Bij.vec)] <- 0
  Bij[,j] <- Bij.vec
  }
  # write.table(Bij, file = "Bij.csv", sep = ",", col.names = NA)
  Bij.data.frame <- data.frame(Bij)
  colnames(Bij.data.frame) <- colnames(allrawdata[,2:ncol(allrawdata)])
  Bij.data.frame <- data.frame(Claim.ID=IDvector,Bij.data.frame)
}