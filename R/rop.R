rop <-
function(fic, output_folder, mini, maxi, nbCycles, typesVariables){

	# Liste des fichiers utiliss
	fichDon <- fic
	#fic = unlist(strsplit(fic, "[./]"))[length(unlist(strsplit(fic, "[./]")))-1]
	#fichRes <- paste(fic, "_res.csv", sep = "")
	#fichSol <- paste(fic, "_sol.csv", sep = "")
	fichRes <- file.path(output_folder, 'result.csv')
	fichSol <- file.path(output_folder, 'solution.csv')

	# Totalisation des vrais et faux ngatifs  la fin des tapes
	totalVnFn <- c(VN = 0, FN = 0)
	
	# Limites de pondrations
	nbVar = length(typesVariables)
	limites<-cbind(rep(mini, nbVar), rep(maxi, nbVar))
	
	# Dtermination du nb de pondrations
	nbPond <- prod(limites[,2] - limites[,1] + 1)

	# lecture des donnes
	donnees<-read.csv2(fichDon,header=T,dec=".")
	# Centrage/Rduction des variables
	for (i in (3:ncol(donnees))){
	  if(typesVariables[i - 2]){
		donnees[,i] <- scale(donnees[,i])
	  }
	}
	var <- colnames(donnees)[3:dim(donnees)[2]]
	cat("=============================================================================\n")
	cat("Factors : ", var, "\n")
	cat("=============================================================================\n")
	ajoutResult(fichRes, c(" ", "Cycle","Etape" , "Nb sujets",var ,"seuil" ,"Se" ,"Sp" ,"Se+Sp" ,"AUC", "VP", "FP", "VN", "FN", "Solution"), FALSE, nbVar)
	ajoutResult(fichSol, c("Cycle", "Etape" , "Id", "Etat", var ,"seuil" ,"Risque" , "Statut", "Selection"), FALSE, nbVar)
	# Vrification du nombre de variables
	nbVarFich = ncol(donnees) - 2
	if (nbVarFich == nbVar) {
	  nbCycles <- executeCycleComplet(donnees, limites, nbCycles, nbPond, fichRes, fichSol, totalVnFn)
	}
	if(nbVarFich != nbVar) {
	  cat(" Nombre de variables du fichier\ndifferent de celui des ponderations\n")
	}
}
