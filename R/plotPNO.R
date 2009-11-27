plotPNO <- function(x, subset = NULL, thinning = NULL, xlab = NULL, tail_threshold = 0, wm = FALSE){
	
	# calculate weighted means:
	# -------------------------
	wmean <- pno.weighted.mean(x, subset = subset)
		
	# subset matrix
	# ---------------------
	if (!is.null(subset))
		x <- x[, c(1, which(names(x) %in% sset))]
		
	for (i in 2:(dim(x)[2])){
		nf <- sum(x[, i])
		x[, i] <- x[, i] / nf
	}
	
	# delete 'zero tails'
	# ---------------------
	zeros <- which(apply(x[, -1], 1, sum) <= tail_threshold)
	if (length(zeros) > 0)
			x <- x[-zeros, ]
			
	# thin matrix
	# --------------------
	if (!is.null(thinning)) 
		x <- x[seq(1, dim(x)[1], length.out = thinning), ]
	
	col <- rainbow(dim(x)[2] - 1)
	max_val  <-  max(x[, -1])
	plot(x[, 1], x[, 2], type = "l", col = col[1], ylim = c(0, 	max_val),
		main = "Predicted niche occupancy",
		xlab = xlab,
		ylab = ""
	)
		
	for (i in 3:(dim(x)[2]))
		lines(x[, 1], x[, i], col = col[i - 1])
	legend(x = min(x[, 1]), y = max(x[, -1]),  			legend = colnames(x)[2:dim(x)[2]], fill = col)
		
	# plot weighted means:
	if (wm){
		for (i in seq(along = wmean))
			lines(rep(wmean[i], 2), range(x[, -1]), col = col[i],				lty = 3, lwd = 3)
	}
}