\name{pno}
\Rdversion{1.1}
\alias{pno}

\title{
Predicted niche occupancy profiles
}

\description{
Following the approach of Evans et al. (2009), \code{pno} integrates species probability (suitability) distributions (e.g. derived with MAXENT) with respect to single climatic (or other environmental) variables in order to get predicted niche occupancy (PNO) profiles for each species and environmenatal variable.
}

\usage{
pno(path_bioclim, path_model, subset = NULL, 
	bin_width = 1, bin_number = NULL)
}

\arguments{
	
  \item{path_bioclim}{The path leading to an ASCII raster map with environmental data (bioclimatic layer) and ArcGIS-compatible header.}
  
  \item{path_model}{The path leading to the directory, where MAXENT probability surfaces are stored as ASCII raster maps with ArcGIS-compatible header.}
  
  \item{subset}{A vector of mode 'character' giving taxon names if only a subset of the species ENMs in \code{path_model} is to be used}
  
  \item{bin_width}{A numeric, giving the bin interval.}
  
  \item{bin_number}{An integer, giving the number of bins used.}
}

\value{
A data frame of cumulative probalities of suitability for each value on an environmental gradient (rows) for the a set of studied taxa (rows). The first column gives the mean of each category.
}

\references{
Evans, M. E. K., S. A. Smith, R. S. Flynn, and M. J. Donoghue. 2009. Climate, niche evolution,
and diversification of the 'bird-cage evening primroses' (\emph{Oenothera}, sections \emph{Anogra} and 
\emph{Kleinia}). \emph{Am. Nat.} \bold{173}: 225-240.
}

\author{
Christoph Heibl (\email{heibl@lmu.de})
}

\seealso{
\code{\link{pno.weighted.mean}}, 
\code{\link{niche.overlap}}, 
\code{\link{anc.clim}}
}

