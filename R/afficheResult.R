afficheResult <-
function(x, noCycle, noEtape, nbSujets, nbVar){
  cat("Cycle no.", noCycle, "\n")
  cat("Step no.", noEtape, "\n")
  cat("Number of observations  : ", nbSujets, "\n")
  cat("Coefficients : ", x[1:nbVar], "\n")
  cat("Threshold=", x["seuil"], "    Se=", x["Se"], "Sp=", x["Sp"], "AUC=", x["AUC"], "\n")
  cat("=============================================================================\n")
}
