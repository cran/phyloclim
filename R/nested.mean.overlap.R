nested.mean.overlap <- function(phy, node, olap){
	
	# match odering of phy and olap
	id <- match(phy$tip.label, rownames(olap))
	olap <- olap[id, id]
	
	# get daughter nodes
	d2 <- phy$edge[phy$edge[, 1] == node, 2]
	# get descendents of both daughter nodes
	set1 <- descendants(phy, d2[1])
	set2 <- descendants(phy, d2[2])
		
	for (j in set1){
		for (k in set2){
			 n <- nbConnectingNodes(phy, c(j, k))
			 o <- 0.5 ^ n * olap[j, k] 
			}
	}
	o
}