niche.equivalency.test <- function(spec, n, maxent, mx = 2000){
	
	# read samples file:
	z <- read.csv(maxent$samples)
	z <- z[z[, 1] %in% spec, ]
	
	# read background file:
	bg <- read.csv(maxent$background)
		
	# create permutated samples file:
	# -------------------------------
	zzz <- NULL
	for (i in 1:n){
		zz <- z
		zz[, 1] <- zz[sample(dim(z)[1]), 1]
		zz[, 1] <- paste(zz[, 1], i, sep = "_")
		zzz <- rbind(zzz, zz)
	}
	testnames <- unique(zzz[, 1])
	
	# save input files:
	# -----------------
	if (file.exists("R.phyloclim.temp"))
	    unlink("R.phyloclim.temp", recursive = TRUE)
	dir.create("R.phyloclim.temp")
	dir.create("R.phyloclim.temp/out")
	
	write.table(bg, "R.phyloclim.temp/background.csv", 
	    row.names = FALSE, col.names = TRUE, sep = ",")
	write.table(rbind(z, zzz), "R.phyloclim.temp/samples.csv",
	    row.names = FALSE, col.names = TRUE, sep = ",")
	
	# call MAXENT:
	# ------------
	mx <- paste("-mx", mx, "m", sep = "")
	call <- paste("java", mx, "-jar", maxent$app , 		"-e R.phyloclim.temp/background.csv",
		"-s R.phyloclim.temp/samples.csv", 
		"-j", maxent$projection, 
		"-o R.phyloclim.temp/out", 			
		"-r removeduplicates nopictures", 
	    "outputformat=raw autorun")
	system(call, wait = TRUE)
	
	# analyse output
	# --------------
	rwd <- getwd()
	setwd("R.phyloclim.temp/out")
	
	projname <- gsub("^.+/", "", maxent$projection)
	projname <- paste(projname, "asc", sep = ".")
	
	# original models:
	# ----------------
	fn <- paste(spec[1], projname, sep = "_")
	x <- import.asc(fn, type = "numeric")
	fn <- paste(spec[2], projname, sep = "_")
	y <- import.asc(fn, type = "numeric")
	
	di <- getDI(x, y)
	
	# null models:
	# ----------------
	nd <- NULL
	for (i in 1:n){
		fn <- paste(spec[1], i, projname, sep = "_")
		x <- import.asc(fn, type = "numeric")
		fn <- paste(spec[2], i, projname, sep = "_")
		y <- import.asc(fn, type = "numeric")
		nd <- rbind(nd, getDI(x, y))
	}
	
	# assess significance:
	# --------------------
	m <- colMeans(nd)
	s <- apply(nd, 2, sd)
	
	p.D <- pnorm(di[1], m[1], s[1])
	p.I <- pnorm(di[2], m[2], s[2])
	
	# change wd and remove MAXENT output:
	# ----------------------------------
	setwd(rwd)
	unlink("R.phyloclim.temp", recursive = TRUE)
	
	# create output object:
	# ---------------------
	list(
		test = "identity",
		spec = spec,
		D = c(di[1], p = p.D), 
		I = c(di[2], p = p.I),
		null.distribution = nd
	)	
}