plotAncClim <- function(x, clades = NULL, col, density = TRUE, lwd = 1, xspace = c(0, 0.1), ylab = ""){
	
	# get data:
	# ---------
	tr <- x$tree
	tips <- tr$tip.label
	nbtips <- length(tips)
	clim <- x$means
	
	if (density)
		cd <- x$central.density
		
	# if no clades are given use terminal sisters 
	# for coloration instead. Species in a grade
	# will receive their own color	
	if (is.null(clades)) {
		ts <- terminal.sisters(tr)
		nts <- tr$tip.label[!tr$tip.label %in% ts]
		for (i in seq(along = nts))
			ts <- rbind(ts, rep(nts[i], 2))
		lts <- vector(mode = "list", length = dim(ts)[1])
		for (i in seq(along = lts))
			lts[[i]] <- ts[i, ]
		clades <- lts
	}
	
	# abbreviate taxon labels:
	# -------------------------
	tips <- tr$tip
	abb <- function(tips){
		tips <- paste(head(unlist(strsplit(tips, "")), 3), 			collapse = "")
		tips
	}
	tips <- sapply(tips, abb)
		
	#
	max_age <- -max(branching.times(tr))

	# calculate x-coordinate for nodes bases on node ages
	# and 'max_age'
	# -------------
	nodeages <- c(rep(0, nbtips), -branching.times(tr))
	
	# coordinates for plotting: node ages versus clima values
	# --------------------------------------------------------
	xy <- cbind(nodeages, clim)
	
	# calculate space needed for tip labels:
	# -------------------------------------
	space <- max(nchar(tips)) * 0.3 + xspace[1]
	
	# plot tiplabels
	# --------------
	tex <- cbind(rep(space * 0.6 , nbtips), xy[1:nbtips, 2])
	rownames(tex) <-  tr$tip.label
	for (i in 0:(length(clades) -1)){
		ind <- which(rownames(tex) %in% clades[[i + 1]])
		xv <- max(tex[, 1]) + seq(0, xspace[2] * (length(ind) - 			1), length.out = length(ind))
		if (i > 0) xv <- xv + space
		 tex[ind, 1] <- xv
	}
	rownames(tex) <-  tips
	#totalspace <- space * length(clades)
	#totalspace <- diff(range(tex[, 1]))
	totalspace <- max(tex[, 1]) + space * 0.5
	
	
	# plot coordinate system
	# ----------------------
	if (!density) yrange <- range(xy[, 2])			else yrange <- c(min(cd[1, ]), max(cd[2, ]))
	plot(c(max_age, totalspace), yrange,
		type = "n",
		xlab = "Time (Ma)", ylab = ylab,
		bty = "c", xaxp = c(floor(max_age), 0, 5))
		
	# edge colors:
	# ------------	
	n <- noi(tr, clades)
	n <- lapply(n, descendants, tree = tr, internal = TRUE)
	lincol <- rep("grey", dim(xy)[1])
	if (missing(col))
		col <- rainbow(length(n))
	for (i in seq(along = n)){
		lincol[n[[i]]] <- col[i]
	}
	
	# plot edges:
	# ----------------
	colind <- vector() # used to order tip colors!
	for (i in seq(along = tr$edge[, 1])){
		ind <- tr$edge[i, ]
		lines(xy[ind, 1], xy[ind, 2], lwd = lwd, 			col = lincol[ind[2]])
		colind <- c(colind, ind[2])
	}
	
	# plot zero line:
	# ----------------
	if (min(xy[, 2]) < 0 & max(xy[, 2] > 0))
	lines(c(min(xy[, 1]), totalspace), rep(0, 2), lty = "11")
	
	# plot tiplabels:
	# ----------------
	text(tex[, 1], tex[, 2], rownames(tex), cex = 0.9, 		adj = 0.5, col = lincol[1:nbtips], font = 4)
	
	# density:
	# ----------------
	if (density){
		for (i in seq(along = cd[1, ]))
		lines(rep(tex[i, 1], 2), cd[, i], 				col = lincol[1:nbtips][i], lwd = 1.5, lty = "11")
	}
}