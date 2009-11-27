noi <- function(tr, group, regex = NULL, monophyletic = FALSE){
	
	# matrix of pairwise MRCA
	# -----------------------
	x <- mrca(tr)
	
	foo <- function(group, tr, regex){
		if (is.null(regex)){
			y <- which(tr$tip.label %in% group)
			group <- tr$tip.label[y]	
			mintax <- group[y == min(y)]
			maxtax <- group[y == max(y)]
		}												else {
			regex<- paste(group, collapse = "|")
			y <- grep(regex, tr$tip.label)
			mintax <- tr$tip.label[min(y)]
			maxtax <- tr$tip.label[max(y)]
		}
		x <- x[rownames(x) == mintax, colnames(x) == maxtax]
		if (monophyletic){
			test <- tr$tip.label[descendants(tr, x)]
			if (!all(test %in% group))
				x <- NA
		}
		x
	}
	if (!is.list(group)) group <- list(group)
	nodes <- unlist(lapply(group, foo, tr = tr, regex = regex))
	nodes
}