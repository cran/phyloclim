plotNicheEquivalency <-
function(x){
	
	# divide screen
	par(mfrow = c(1, 2))
	
	# D
	par(mar = c(5, 3, 2, 1))
	f <- hist(x$n[, 1], xlim = c(0, 1), col = "grey80", main = NULL, 		xlab = "D")
	y <- 0.5 * max(f$c)
	lines(rep(x$D[1], 2), c(0, y), col = "red")
	text(x$D[1], y * 1.05, round(x$D[1], digits = 3), adj = 0.5)
	
	# I
	par(mar = c(5, 1, 2, 1))
	f <- hist(x$n[, 2], xlim = c(0, 1), col = "grey80", main = NULL,		xlab = "I")
	y <- 0.5 * max(f$c)
	lines(rep(x$I[1], 2), c(0, y), col = "red")
	text(x$I[1], y * 1.05, round(x$I[1], digits = 3), adj = 0.5)
	# add title
	par(mfrow = c(1, 1))
	title(paste("Niche identity test:", paste(x$spec, 		collapse = " - ")))
}