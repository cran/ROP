decoupeRnonR <-
function(donnees, coeff, seuil, noCycle, noEtape, nbVar, fichSol){
  # Application des coeff de pondration
  tab <- t(apply(donnees, MARGIN=1, FUN=appliquePonder, c(1, 1, coeff)))
  # Calcul des scores
  tab <- cbind(tab, score=apply(tab[, 3:ncol(tab)], MARGIN=1, FUN="sum"), risque=0)
  tab[,"score"] <- round(tab[,"score"], 7)
  # Reprage des sujets  risque
  idxRisque = which(tab[,"score"] >= round(seuil, 7))
  tab[,"risque"][idxRisque] <- 1
  for (i in (1:nrow(tab)))
	ajoutResult(fichSol,c(noCycle, noEtape, tab[i,], evalueStatut(tab[i, 2], tab[i, "risque"])), TRUE, nbVar)
  # Slection des sous-populations
  sujetsRisque <- donnees[idxRisque,]
  sujetsNonRisque <- donnees[-idxRisque,]
  return(list(risque=sujetsRisque, nonRisque=sujetsNonRisque))
}
