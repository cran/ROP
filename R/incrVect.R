incrVect <-
function(v, limites){
  taille <- length(v)
  for (i in (1:(taille-1))){
    # Traitement en cascade de chaque lment du vecteur
    r <-incrVal(v[i], limites[i, 1], limites[i, 2])
    # Actualisation de l'lment
    v[i] <- r[1]
    # Prise en compte de la retenue
    v[i+1] <- v[i+1] + r[2]
    if (v[i+1] <= limites[i+1,2])
      # si l'lment suivant n'est pas suprieur  la limite, arrt du traitement
      break
  }
  return (v)
}
