age.range.correlation <- 
function(phy, overlap, tri = "upper", n = 1000){
	
	# check input
	# -----------
	if (!inherits(phy, "phylo")) 
        stop("object \"phy\" is not of class \"phylo\"")
			
	# ages:
	# -----
	age <- branching.times(phy)	
	
	# make matrix symmetrical
	# -----------------------
	ovlap <- overlap
	if (tri == "upper")
		ovlap[lower.tri(ovlap)] <- t(ovlap)[lower.tri(ovlap)]
	if (tri == "lower")
		ovlap[upper.tri(ovlap)] <- t(ovlap)[upper.tri(ovlap)]
	
	# match matrix to tree
	# --------------------
	id <- match(phy$tip.label, rownames(ovlap))
	ovlap <- ovlap[id, id]
	
	# calculate 'nested mean overlap'
	# -------------------------------
	overlap <- sapply(names(age), nested.mean.overlap, phy = phy, 		olap = ovlap)

	x <- cbind(age, overlap)	
	x.lm <- lm(overlap ~ age)
	
	# randomization:
	# --------------
	randomization <- function(phy, o, n, age){
		id <- sample(seq(along = o[, 1]))
		rownames(o) <- colnames(o) <- colnames(o)[id]
		o <- sapply(names(age), nested.mean.overlap, phy = phy, 			olap = o)
		o <- cbind(age, o)
		o <- lm(o[, 2] ~ age)
		o$coefficients
	}
	reps <- 1:n
	random.x <- sapply(reps, randomization, o = ovlap, phy = phy, 		age = age)
	
	f.intercept <- 										length(which(random.x[1,] > x.lm$coefficients[1]))/n
	f.slope <- 											length(which(random.x[2,] > x.lm$coefficients[2]))/n
	
	f <- c(f.intercept, f.slope)
	p <- sapply(f, function(x) 2 * min(x, 1 - x))
	sig <- cbind(f, p)
	rownames(sig) <- c("intercept", "slope")
	
	list(age.range.correlation = x, 
		linear.regression = x.lm, 
		sig = sig,
		MonteCarlo.replicates = t(random.x) 
			
	)
	
}