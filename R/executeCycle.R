executeCycle <-
function(matDonCycle, limites, noCycle, nbPond, fichRes, fichSol, totalVnFn){
  # Etape 1 : Analyse de l'ensembles des sujets
  donnees <- executeEtape(matDonCycle, limites, noCycle, 1, nbPond, fichRes, fichSol)
  sujetsRisque <- donnees$risque
  sujetsNonRisque <- donnees$nonRisque 
  # Etape 2 : Analyse des sujets considrs sans risque
  donnees <- executeEtape(sujetsNonRisque, limites, noCycle, 2, nbPond, fichRes, fichSol)
  # Combinaison des sujets  risque reprs lors des 2 tapes
  resultat <- rbind(sujetsRisque, donnees$risque)
  # Calcul du Se/Sp final
  f <- read.csv2(fichRes, header=T, dec=".")
  nbVar <- ncol(matDonCycle) - 2 
  e1 <- f[nrow(f)-1, c(4, (nbVar + 10):(nbVar + 11))]
  e2 <- f[nrow(f), c(4, (nbVar + 10):(nbVar + 11))]
  e3 <- f[nrow(f), c((nbVar + 12):(nbVar + 13))]
  totalVnFn["VN"] <- totalVnFn["VN"] + e3$VN
  totalVnFn["FN"] <- totalVnFn["FN"] + e3$FN
  e <- c((e1$Nb.sujets + e2$Nb.sujets), VP = (e1$VP + e2$VP), FP = (e1$FP + e2$FP), totalVnFn["VN"], totalVnFn["FN"])
  eSe <- e["VP"] / (e["VP"] + e["FN"]) * 100
  eSp <- e["VN"] / (e["VN"] + e["FP"]) * 100
  NbSujets <- sum(e[2:5])
  arret <- ((eSe == 0) || (eSp == 0))
  cat("*****************************************************************************\n")
  cat("   ->  Sensitivity = ", unlist(eSe), "Specificity = ", unlist(eSp), "\n")
  cat("*****************************************************************************\n")
  cat("=============================================================================\n")
  
  ajoutResult(fichRes, unlist(c("RA", noCycle, " ", NbSujets, rep(" ", nbVar), " ", eSe, eSp, rep(" ", 2), e["VP"], e["FP"], e["VN"], e["FN"])), TRUE, nbVar)
  return(list(resultat, arret, totalVnFn))  
}
