ajoutResult <-
function(ficNom, resultAjout, ajout=TRUE, nbVar){
  write(resultAjout, ficNom, ncolumns=nbVar+14, append=ajout, sep=";")  
}
