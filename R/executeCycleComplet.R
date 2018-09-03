executeCycleComplet <-
function(matDonComplet, limites, iterations, nbPond, fichRes, fichSol, totalVnFn){
  i <- 0
  arret <- FALSE
  while (!arret && (i < iterations)){
    i <- i + 1
    resCycle <- executeCycle(matDonComplet, limites, i, nbPond, fichRes, fichSol, totalVnFn)
    matDonComplet <- resCycle[[1]]
    arret <- resCycle[[2]]
	totalVnFn <- resCycle[[3]]
  } 
  cat("Analysis performed in ", i, "cycles\n")
  return(i)
}
