calculROC <-
function (matDonCalc, dernierSeSp, dernierAUC){
  # Calcul des scores
  scores <- cbind(matDonCalc[,1:2], round(apply(matDonCalc[,3:ncol(matDonCalc)], MARGIN=1, FUN=sum), 7), c(0))
  colnames(scores) <- c("no", "ETAT", "score", "risque")
  # Choix du seuil
  pred <- prediction(scores[, "score"], scores[, "ETAT"])
  Se <- performance(pred, measure="sens")@y.values
  Sp<- performance(pred, measure="spec")@y.values
  mat <-cbind(pred@cutoffs[[1]], Se[[1]], Sp[[1]], Se[[1]] + Sp[[1]])
  seuil <- round(mat[which(mat[, 4] == max(mat[, 4])), 1][[1]], 7)
  # Calcul de l'aire sous courbe
  AUC <- performance(pred,measure="auc")@y.values
  # Calcul sensibilit / spcificit
  resSeSp <- calculSeSp(scores, seuil)
  sensibilite <- resSeSp["Sens"]
  specificite <- resSeSp["Spec"]
  resultats <- rep(0, 5)
  if ((sensibilite + specificite) > dernierSeSp){
    # Nouveau maximum -> raz du tableau des rsultats
    resultats <- list("RAZ", c(seuil, sensibilite, specificite, sensibilite+specificite, AUC[1], resSeSp["VP"], resSeSp["FP"], resSeSp["VN"], resSeSp["FN"]))
  }
  else {
    if (((sensibilite+specificite) == dernierSeSp) && (AUC[[1]] == dernierAUC)){ 
      # Rsultat gal au maximum et aire sous courbe identique -> ajout du rsultat
      resultats <- list("AJ", c(seuil, sensibilite, specificite, sensibilite+specificite, AUC[1], resSeSp["VP"], resSeSp["FP"], resSeSp["VN"], resSeSp["FN"]))    
    }
    else {
      if (((sensibilite+specificite) == dernierSeSp) && (AUC[[1]] > dernierAUC)){
        # Nouvelle aire sous courbe maximale
        resultats <- list("RAZ", c(seuil, sensibilite, specificite, sensibilite+specificite, AUC[1], resSeSp["VP"], resSeSp["FP"], resSeSp["VN"], resSeSp["FN"]))
      }
    }
  }
  return(resultats)
}
