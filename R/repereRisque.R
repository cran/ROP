repereRisque <-
function(matDonRisque, seuilRisque){
  matRisque <- cbind(matDonRisque, risque=0)
  idx <- which(matRisque[,"score"] >=seuilRisque)
  matRisque[,"risque"][idx] <- 1
  return(matRisque)
}
