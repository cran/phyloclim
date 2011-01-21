\name{hypothesis.testing}
\Rdversion{1.1}
\alias{niche.equivalency.test}
\alias{bg.similarity.test}

\title{Niche equivalency and background similarity test}

\description{
Hypothesis testing as proposed by Warren et al. (2008) based on the generation of pseudoreplicate datasets. The niche equivaleny (or identity) test asks whether the ecological niche models (ENMs) of two species are more different than expected if they are drawn from the same underlying distribution. The background similarity test asks whether ENMs drawn from populations with partially or entirely non-overlapping distributions are any more different from one another than expected by random chance. 
}

\usage{
niche.equivalency.test(spec, n, maxent, mx = 2000)

bg.similarity.test(spec, n, maxent, mx = 2000)
}

\arguments{
  \item{spec}{A vector of mode \code{character} and length = 2 that corresponds to two species names in the sample file (see details).}
  
  \item{n}{An integer given the number of permutations of the original data.}
  
  \item{maxent}{A list containing the location of the MAXENT application and its input files (see details).}
  
  \item{mx}{An integer giving the number of megabytes of memory made available to JAVA.}
}

\details{
An installation of MAXENT (Phillips et al., 2006; \url{http://www.cs.princeton.edu/~schapire/maxent/}) is required in order to run \code{niche.equivalency.test} and \code{bg.similarity.test}. The \code{maxent} argument of both functions is a list of four elements:

\describe{
\item{app}{The path to the MAXENT application.}
\item{samples}{The path to a SWD-formatted file with sample points.}
\item{background}{The path to a SWD-formatted file with background points.}
\item{projections}{The path to a folder containing environmental GIS layers to be used for projection of the MAXENT models.}
}

For an explanation of SWD-formatted (=Samples-With-Data) files and model projection see the MAXENT tutorial.
}

\value{
A list containing five elements:
\item{test}{Name of the test.}
\item{spec}{Names of the two species compared.}
\item{D}{Measure of niche overlap D based on Schoeners D together with p-values.}
\item{I}{Measure of niche overlap I based on Hellinger distances together with p-values.}
\item{null.distribution}{Null distributions of D and I derived from randomization.}
}

\references{	
Phillips, S.J, M. Dudik, & R.E. Schapire. 2006. Maximum entropy modeling of species geographic distributions. \emph{Ecological Modeling} \bold{190}: 231-259.	
	
Warren, D., R.E. Glor, & M. Turelli. 2008. Environmental niche equivalency versus conservatism: quantitative approaches to niche evolution. \emph{Evolution}. \bold{62}: 2868-2883.
}

\author{Christoph Heibl}

\note{These functions have been developed with MAXENT version 3.3.1. and subsequently updated to work with versions 3.3.2 and 3.3.3}

\seealso{\code{\link{niche.overlap}}}