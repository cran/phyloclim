bg.similarity.test <- function(spec, n, maxent, mx = 2000){
	
	# read samples file:
	z <- read.csv(maxent$samples)
	if (length(unique(z[, 1])) == 2)
		spec <- unique(z[, 1])
	z <- z[z[, 1] %in% spec, ]
	
	# read background file:
	bg <- read.csv(maxent$background)
	
	# datapoints per species
	# ----------------------
	o <- table(z[, 1])
	o <- o[o > 0]
	o <- o[match(spec, names(o))]
	
	# random samples from background
	# ------------------------------
	rb <- NULL
	for (i in 1:n){
		# first species
		id <- sample(seq(along = bg[, 1]), o[1])
		bg_temp <- bg[id, ]
		bg_temp[, 1] <- paste(names(o[1]), i, sep = "_")
		rb <- rbind(rb, bg_temp)
		# second species
		id <- sample(seq(along = bg[, 1]), o[2])
		bg_temp <- bg[id, ]
		bg_temp[, 1] <- paste(names(o[2]), i, sep = "_")
		rb <- rbind(rb, bg_temp)
	}
	testnames <- unique(rb[, 1])
	
	# save input files:
	# -----------------
	if (file.exists("R.phyloclim.temp"))
	    unlink("R.phyloclim.temp", recursive = TRUE)
	dir.create("R.phyloclim.temp")
	dir.create("R.phyloclim.temp/out")
	
	write.table(bg, "R.phyloclim.temp/background.csv", 
	    row.names = FALSE, col.names = TRUE, sep = ",")
	write.table(rbind(z, rb), 
	    "R.phyloclim.temp/samples.csv", row.names = FALSE, 
	    col.names = TRUE, sep = ",")
	
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
	xorig <- import.asc(fn, type = "numeric")
	fn <- paste(spec[2], projname, sep = "_")
	yorig <- import.asc(fn, type = "numeric")
	
	di <- getDI(xorig, yorig)
	
	# null models:
	# ----------------
	nd <- NULL
	for (i in 1:n){
		fn <- paste(spec[1], i, projname, sep = "_")
		x <- import.asc(fn, type = "numeric")
		fn <- paste(spec[2], i, projname, sep = "_")
		y <- import.asc(fn, type = "numeric")
		nd <- rbind(nd, c(getDI(xorig, y), getDI(yorig, x)))
	}
	
	# assess significance:
	# --------------------
	m <- colMeans(nd)
	s <- apply(nd, 2, sd)
	di <- rep(di, 2)
	less.sim <- di < m
	
	p.D.xBacky <- pnorm(di[1], m[1], s[1], less.sim[1])
	p.D.yBackx <- pnorm(di[3], m[3], s[3], less.sim[3])
	p.I.xBacky <- pnorm(di[2], m[2], s[2], less.sim[2])
	p.I.yBackx <- pnorm(di[4], m[4], s[4], less.sim[4])
	
	# change wd and remove MAXENT output:
	# ----------------------------------
	setwd(rwd)
	unlink("R.phyloclim.temp", recursive = TRUE)
	
	# create output object:
	# ---------------------
	list(
		test = "background.similarity",
		spec = spec,
		D = c(di[1], p.X_backY = p.D.xBacky, 				p.Y_backX = p.D.yBackx), 
		I = c(di[2], p.X_backY = p.I.xBacky, 				p.Y_backX = p.I.yBackx),
		null.distribution = nd
	)	
}