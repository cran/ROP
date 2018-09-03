analyse <-
function (donneesAna, cPondAna, limites, nbPond){
  # Initialisations du tableau de rsultats et de la somme Se+Sp
  resultats <- NULL
  SommeSeSp <- 0
  AireSousCourbe <- 0
  noPond <- 1
  while (noPond <= nbPond){
    # Application des pondrations
    fichPond <- t(apply(donneesAna, MARGIN=1, FUN=appliquePonder, c(1, 1, cPondAna)))
    # Calcul des scores
    result <- calculROC(fichPond, SommeSeSp, AireSousCourbe)
	action = result[[1]]
    valeurs = result[[2]]
    # Stockage des rsultats
    if (action == "RAZ"){
      resultats <- t(as.matrix(c(cPondAna, valeurs)))
      # Stockage du dernier rsultat Se+Sp
      SommeSeSp <- valeurs[4]
      AireSousCourbe <- valeurs[5]
    }
    else{
      if (action == "AJ"){
        resultats <- rbind(t(as.matrix(c(cPondAna, valeurs))), resultats)
      }
    }
    # Dtermination des nouveaux coefficients de pondration
    cPondAna <- incrVect(cPondAna, limites)
    noPond <- noPond + 1
  }  
  return (resultats)
}
