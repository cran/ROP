evalueStatut <-
function(Etat, Risque){
	if (Etat == 1)
		if (Risque == 1)
			statut <- "VP"
		else
			statut <- "FN"
	else
		if (Risque == 1)
			statut <- "FP"
		else
			statut <- "VN"
			
	return(statut)
}
