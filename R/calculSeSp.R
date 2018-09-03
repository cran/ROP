calculSeSp <-
function (matSeSp, seuilSeSp){
  if(length(colnames(matSeSp)[colnames(matSeSp)=="score"])==0){
     matSeSp <- cbind(matSeSp, score=round(apply(matSeSp[,3:ncol(matSeSp)], MARGIN=1, FUN=sum), 7))
  }
  matSeSp <- cbind(matSeSp, risque=0)
  idx <- which(matSeSp[, "score"] >= seuilSeSp)
  matSeSp[, "risque"][idx] <- 1
  vp <- length(which(matSeSp[,"ETAT"]==1 & matSeSp[,"risque"]==1))
  fp <- length(which(matSeSp[,"ETAT"]==0 & matSeSp[,"risque"]==1))
  vn <- length(which(matSeSp[,"ETAT"]==0 & matSeSp[,"risque"]==0))
  fn <- length(which(matSeSp[,"ETAT"]==1 & matSeSp[,"risque"]==0))
  if (vp == 0){fn <- 1}
  if (vn == 0){fp <- 1}
  Se <- vp / (vp + fn) * 100
  Sp <- vn / (vn + fp) * 100
  return(c(Sens=Se, Spec=Sp, VP=vp, FP=fp, VN=vn, FN=fn))
}
