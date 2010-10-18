niche.overlap <- 
function(x){
	
	# CASE 1: x is a pno matrix
	# -------------------------
	if (class(x) == "data.frame"){
		x <- x[, -1]
		nspec <- dim(x)[2]
		DI <- matrix(nrow = nspec, ncol = nspec)
		rownames(DI) <- colnames(DI) <-colnames(x)
		for (i in 1:(nspec - 1)){
			for (j in (i + 1):nspec){
				dhi <- getDI(x[, i], x[, j])
				DI[i, j] <- dhi[1]
				DI[j, i] <- dhi[2]
			}
		}
	}

	# CASE 2: is vector of filenames
	# ------------------------------
	if (class(x) == "character"){
		nspec <- length(x)
		DI <- matrix(nrow = nspec, ncol = nspec)
		rownames(DI) <- names(x)
		colnames(DI) <- names(x)
		for (i in 1:(nspec - 1)){
			X <- import.asc(x[i], type = "numeric")
			for (j in (i + 1):nspec){
				Y <- import.asc(x[j], type = "numeric")
				dhi <- getDI(X, Y)
				DI[i, j] <- dhi[1]
				DI[j, i] <- dhi[2]
			}
		}
	}
	
	# CASE 3: x is a list of 'asc' objects
	# ------------------------------------
	if (class(x[[1]]) == "asc"){
		nspec <- length(x)
		DI <- matrix(nrow = nspec, ncol = nspec)
		rownames(DI) <- names(x)
		colnames(DI) <- names(x)
		for (i in 1:(nspec - 1)){
			for (j in (i + 1):nspec){
				dhi <- getDI(x[[i]], x[[j]])
				DI[i, j] <- dhi[1]
				DI[j, i] <- dhi[2]
			}
		}
	}
	class(DI) <- "niolap"
	DI
}