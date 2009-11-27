pno <-  
function(path_bioclim, path_model, subset = NULL, bin_width = 1, 	bin_number = NULL){
	
	# read and reorganize BIOCLIM layer: slow
	# ---------------------------------------
	bioclim <- import.asc(path_bioclim, type = "numeric")
	
	
	
	# create bins for climate values:
	# -------------------------------
	bc_min <- min(bioclim, na.rm = TRUE)
	bc_max <- max(bioclim, na.rm = TRUE)
	if (is.null(bin_number)) by <- bin_width		else by = ((bc_max - bc_min)/(bin_number - 1))
	cats  <- seq(from = bc_min, to = bc_max, by = by)
	cat("\nData ranges from", bc_min, "to", bc_max, 		"and will be binned into\n", length(cats), 		"categories with a bin width of", by)
	
	# get species names
	specs <- list.files(path = path_model, pattern = ".asc", 		full.name = TRUE)
	# clamping maps:
	clamping <- grep("clamping", specs)
	if (length(clamping) > 0)
		specs <- specs[-clamping]
	if (!is.null(subset)){
		specs <- specs[grep(paste(subset, collapse = "|"), specs)]
	}
	
	# get taxon names
	label <- gsub(path_model, "", specs)
	label <- gsub("/", "", label)
	label <- gsub(".asc", "", label)
		
	# initialize matrix to store results
	# ----------------------------------
	x <- matrix(nrow = length(cats), ncol = length(specs) + 1)
	colnames(x) <- c("variable", label)
	x[, 1] <- cats
	
	# loop over MAXENT models (MM)
	# ----------------------------
	for (h in seq(along = specs)) {
		cat("\nReading ENM for", specs[h])
		ENM <- import.asc(specs[h], type = "numeric")
		
		if (!identical(dim(bioclim), dim(ENM)))
			stop("Resolution and/or extent of ENM and", 				" bioclimatic layer do not match!")
		
		normalizer <- sum(ENM, na.rm = TRUE)
	
		# calculate cumulative probabilities for each value
		# on the environmental gradient
		# -----------------------------
		cum.prob.bin <- function(x, bc, enm, normalizer, 			bin){
			x <- enm[which(bc > x - bin/2 & bc <= x + bin/2,
				arr.ind = TRUE)] 			
			sum(x) / normalizer
		}
		cat("\nBinning probabilities for", specs[h])
		prob <- sapply(cats, cum.prob.bin, bc = bioclim, 
			enm = ENM, normalizer = normalizer, bin = by)
		
		# remove ENM
		# ----------------------------
		remove(ENM)
		
		# fill data into matrix
		# ---------------------
		x[, h + 1] <- prob
			
	}
	# original scale of Temperature values:
	# -------------------------------------
	if (length(grep("Temperature|Diurnal", path_bioclim)) 		== 1)
		x[, 1] <- x[, 1] / 10
	if (length(grep("Temperature_Seasonality|Isothermality", 		path_bioclim)) 		== 1)
		x[, 1] <- x[, 1] / 100
	x
}