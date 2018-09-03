calculMinValAbs <-
function(serie){
  absolu <- rowSums(abs(serie))
  listeAbsolu<- which(absolu==min(absolu))
  if (length(listeAbsolu)==1){
    pondRetenue <- serie[listeAbsolu,]
    aleat <- FALSE
  }
  else {
    # tirage au sort du jeu de pondrations retenu
    aleat <- TRUE
    idx <- sample(1:length(listeAbsolu), 1)
    pondRetenue <- serie[listeAbsolu[idx],]
  }
  return(list(pondRetenue, aleat))
}
