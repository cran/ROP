listePonder <-
function(limites){
  if (is.vector(limites)){
    liste <- as.list(limites[1]:limites[2]) 
  }
  else {
    liste <- NULL
    nbPond <- prod(limites[,2] - limites[,1] + 1)
    nvPond <- limites[,1]
    i <- 1
    while (i <= nbPond){
      liste <- c(liste, list(nvPond))
      nvPond <- incrVect(nvPond, limites)
      i <- i + 1
    }
  }
  return(liste)
}
