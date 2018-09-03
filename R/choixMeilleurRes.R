choixMeilleurRes <-
function (donnees,matRes, nbVar, var){ 
  # Regroupement des rsultats
  resultats <- NULL
  solution <- 1
  for(i in (1:length(matRes))){
    resultats <- rbind(resultats, matRes[[i]])
  }
  resultats <- matrix(unlist(resultats), ncol=nbVar+9, byrow=FALSE)
  colnames(resultats) <- c(var, "seuil", "Se", "Sp", "Se+Sp", "AUC", "VP", "FP", "VN", "FN")  
  meilleureSeSp <- resultats[which(resultats[,"AUC"] == max(resultats[,"AUC"])),]   
  if (is.matrix(meilleureSeSp)){  
    # plusieurs solutions aboutissent au mme score
    #   -> on choisit celle ayant le meilleur score Se+Sp
    meilleureSeSp <- meilleureSeSp[which(meilleureSeSp[,"Se+Sp"] == max(meilleureSeSp[,"Se+Sp"])),]  
    if(is.matrix(meilleureSeSp)){
      # s'il persiste plusieurs solutions, on vrifie si les solutions portent sur les mmes individus
      idem <- memesSujets(donnees, meilleureSeSp)
      # on recherche la solution dont la somme des valeurs absolues est la plus faible
      resMin <- calculMinValAbs(meilleureSeSp)
      meilleureSeSp <- resMin[[1]]
      tirage <-resMin[[2]]
      solution <- solution + ifelse(idem, 1, 3) + ifelse(tirage, 1, 0)
    }
  }
  meilleureSeSp <- c(meilleureSeSp, Solution=solution)
  return(meilleureSeSp)
}
