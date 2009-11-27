plotBackgroundSimilarity <-
function(x){
	
	par(mfrow = c(2, 2))
	
	# 
	par(mar = c(1, 5, 5, 2))
	f <- hist(x$n[, 1], xlim = c(0, 1), col = "grey80", 		main = NULL, xlab = "D", ylab = 				paste(x$spec[1], "vs. bg of", x$spec[2]))
	y <- 0.5 * max(f$c)
	lines(rep(x$D[1], 2), c(0, y), col = "red")
	text(x$D[1], y * 1.05, round(as.numeric(x$D[1]), digits = 3), 		adj = 0.5)
	
	par(mar = c(1, 2, 5, 2))
	f <- hist(x$n[, 2], xlim = c(0, 1), col = "grey80", 		main = NULL, xlab = "I")
	y <- 0.5 * max(f$c)
	lines(rep(x$I[1], 2), c(0, y), col = "red")
	text(x$I[1], y * 1.05, round(as.numeric(x$I[1]), digits = 3), 		adj = 0.5)
	
	par(mar = c(5, 5, 1, 2))
	f <- hist(x$n[, 3], xlim = c(0, 1), col = "grey80", 		main = NULL, xlab = "D", ylab = 				paste(x$spec[2], "vs. bg of", x$spec[1]))
	y <- 0.5 * max(f$c)
	lines(rep(x$D[1], 2), c(0, y), col = "red")
	text(x$D[1], y * 1.05, round(as.numeric(x$D[1]), digits = 3), 		adj = 0.5)
	
	par(mar = c(5, 2, 1, 2))
	f <- hist(x$n[, 4], xlim = c(0, 1), col = "grey80", 		main = NULL, xlab = "I")
	y <- 0.5 * max(f$c)
	lines(rep(x$I[1], 2), c(0, y), col = "red")
	text(x$I[1], y * 1.05, round(as.numeric(x$I[1]), digits = 3), 		adj = 0.5)
	par(mfrow = c(1, 1))
	par(mar = c(0, 0, 4, 0))
	title(paste("Background similarity test:", paste(x$spec, 		collapse = " - ")))
}