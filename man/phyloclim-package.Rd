\name{phyloclim-package}
\alias{phyloclim-package}
\alias{phyloclim}

\docType{package}

\title{Phyloclimatic data analysis}

\description{
This package implements some recently developed methods in phyloclimatic modeling.
}

\details{
\tabular{ll}{
 Package: \tab phyloclim\cr
 Type: \tab Package\cr
 Version: \tab 0.0.1\cr
 Date: \tab 2009-11-27\cr
 License: \tab GPL (>= 2)\cr
}

\code{phyloclim} is intended to provide methods for the quantification of niche occupancy and niche overlap and combine this data with phylogenetic trees. 

Niche overlap can be quantified either from profiles of environmental suitability for a given environmental variable (PNO or predicted niche occupancy profiles, see \code{\link{pno}}) or from two-dimensional suitability surfaces (environmental niche models (ENMs) or species distribution models (SDMs)) with \code{\link{niche.overlap}}. Randomization tests for testing the significance of  niche overlap in terms of niche identity and niche similarity are available in \code{\link{niche.equivalency.test}} and \code{\link{bg.similarity.test}}.

Phylogenetic data can be integrated into comparative methods such as \code{\link{age.range.correlation}} and the estimation of ancestral environmental tolerances (\code{\link{anc.clim}}), which is a non-parametric approach based on resampling of PNO profiles.

The functions in \code{phyloclim} are in an early stage of development, so they should be used with care! Any feedback is welcome!

}

\author{
Maintainer: Christoph Heibl \email{heibl@lmu.de}
}

\keyword{package}

\seealso{
\code{\link[ape:ape-package]{ape}}
}