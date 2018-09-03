executeEtape <-
function(matDonEtape, limites, noCycle, noEtape, nbPond, fichRes, fichSol){
 # Reprage de la pondration de dpart
  dPond<- limites[,1]
  res <- analyse(matDonEtape, dPond, limites, nbPond)
  nbSujets <- nrow(matDonEtape)
  nbVar <- ncol(matDonEtape) - 2
  var <- colnames(matDonEtape)[3:ncol(matDonEtape)]
  # Slection du meilleur rsultat
  res <- choixMeilleurRes(matDonEtape, res, nbVar, var)
  afficheResult(res, noCycle , noEtape, nbSujets, nbVar)
  ajoutResult(fichRes, unlist(c("A", noCycle, noEtape, nbSujets, res)), TRUE, nbVar)
  # Rcupration des sous-populations risqu/non risqu
  donneesRnonR <- decoupeRnonR(matDonEtape, res[1:nbVar], res["seuil"], noCycle, noEtape, nbVar, fichSol)
  return(donneesRnonR)
}
