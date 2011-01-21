\name{plotAncClim}
\Rdversion{1.1}
\alias{plotAncClim}

\title{Chronograms with climatic data on the y-axis}

\description{This function can be used to plot the history of climatic tolerance for a clade \emph{sensu} Evans et al. (2009).}

\usage{
plotAncClim(x, clades = NULL, col, density = TRUE, lwd = 1,
	xspace = c(0, 0.1), ylab = "")
}

\arguments{
  \item{x}{A list with elements \code{tree}, \code{data}, and (optional) \code{central.density} (see details).}
  
  \item{clades}{A list containing vectors of tip labels which define  the clades to highlight.}
  
  \item{col}{A vector containing colors for different clades.}
  
  \item{density}{Logical, if \code{TRUE}, the central density intervals for recent species are plotted.}
  
  \item{lwd}{The line width, a positive number, defaulting to 1.}
  
  \item{xspace}{A vector of mode \code{numeric}; the first element controls the space between tiplabels of different clades, the second element sets the spaces between tiplabels within clades.}
  
  \item{ylab}{A character string, giving a label for the y-axis, i.e., for the bioclimatic dimension of the plot.}
}

\details{
	
The main argument \code{x} is a list consisting of at least the first two of the following elements: (1) \code{tree} is an ultrametric phylogenetic tree stored as object of class \code{phylo}; (2) \code{data} is an object of class \code{matrix}; its columns correspond to bioclimatic variables and its rows corresond to node numbers such as defined by class \code{phylo} objects; (3) \code{central.density} must only be included if \code{density = TRUE} -- it is a list, which for every bioclimatic variable, contains a matrix that contains the some minimum and maximum quantile of the respective bioclimatic variable for every tip in the tree.	
}


\references{
Evans, M. E. K., S. A. Smith, R. S. Flynn, and M. J. Donoghue. 2009. Climate, niche evolution,
and diversification of the 'bird-cage evening primroses' (\emph{Oenothera}, sections \emph{Anogra} and 
\emph{Kleinia}). \emph{Am. Nat.} \bold{173}: 225-240.
}

\author{Christoph Heibl}

\seealso{
\code{\link{pno}}, \code{\link{pno.weighted.mean}}, \code{\link{anc.clim}}
}

\examples{
# load phylogeny and PNOs of Oxalis sect. Palmatifoliae
data(palmatifoliae_tree)
data(palmatifoliae_pno)

# choose summer precipitation for analysis
clim <- palmatifoliae_pno$PrecipitationWarmestQuarter

# estimate ancestral tolerances
ac <- anc.clim(target = palmatifoliae_tree, pno = clim, n = 100)

# visualize results
plotAncClim(ac)	
}