memesSujets <-
function(matDon,matPond){
  nbPond <- nrow(matPond)
  i <- 1
  nbSujets <- nrow(matDon)
  nbVar <- ncol(matDon) - 2
  somme <- rep(0, nbSujets)
  while (i <= nbPond){
    matDonMemes <- t(apply(matDon, MARGIN=1, FUN=appliquePonder, c(1, 1, matPond[i, 1:nbVar])))
    matDonMemes <- cbind(matDonMemes[,1:2], score=round(apply(matDonMemes[,3:ncol(matDonMemes)], MARGIN=1, FUN=sum), 7))
    matDonMemes <- repereRisque(matDonMemes, matPond[i, "seuil"])
    somme <- somme + matDonMemes[,"risque"]
    i <- i + 1
  }
  Memes <- length(which(somme!=0 && somme!=nbPond)) == 0
  return(Memes)
}
