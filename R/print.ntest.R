print.ntest <- function(x, ...){
  
  if (x$method == "niche identity test"){
    cat(x$method, "\n\n")
    cat("species:", x$species, "\n\n")
    cat("D = ", x$statistic[1], ", p = ", x$p.value[1], "\n", sep = "")
    cat("I = ", x$statistic[2], ", p = ", x$p.value[2], "\n", sep = "")
    cat("(p-value based on", nrow(x$null.dist) + 1, "permutations)\n")
    cat("alternative hypothesis:", x$null)
  }
  if (x$method == "background similarity test"){
    cat(x$method, "\n\n")
    cat("species:", paste(c("(X)", "  (Y)"), x$species), "\n\n")
    cat(paste(rep("\t", 8), sep = ""), "---------- confidence intervals ---------\n")
    cat(paste(rep("\t", 8), sep = ""), "X versus random Y\t\t\t\t\tY versus random X\n")
    
    cat("D = ", x$statistic[1], "   [", 
        paste(round(x$ci.x.randomY[, 1], 5), collapse = ", "), "]   [", 
        paste(round(x$ci.y.randomX[, 1], 5), collapse = ", "), "]\n", sep = "")
    cat("I = ", x$statistic[2],  "   [", 
        paste(round(x$ci.x.randomY[, 2], 5), collapse = ", "), "]   [", 
        paste(round(x$ci.y.randomX[, 2], 5), collapse = ", "), "]\n", sep = "")
    cat("(confidence intervals based on", nrow(x$nd.x.randomY) + 1, "permutations)\n")
    cat("alternative hypothesis:", x$null)
  }
}