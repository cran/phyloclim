\name{adenophylla_arenaria}
\Rdversion{1.1}
\alias{equ}
\alias{sim}
\docType{data}
\title{
Niche overlap for Oxalis adenophylla and O. arenaria
}
\description{
Objects \code{equ} and \code{sim} contain the results of the \code{\link{niche.equivalency.test}} and the \code{\link{bg.similarity.test}} (Warren et al. 2008) for \emph{Oxalis adenophylla} and \emph{O. arenaria} from section \emph{Palmatifoliae}.
}
\usage{
data(equ)
data(equ)}
\format{
A list containing five elements:

\describe{
 \item{test}{Name of the test.}
 \item{spec}{Names of the two species compared.}
 \item{D}{Measure of niche overlap D based on Schoeners D together with p-values.}
 \item{I}{Measure of niche overlap I based on Hellinger distances together with p-values.}
 \item{null.distribution}{Null distributions of D and I derived from randomization.}
}
}

\source{
C. Heibl, unpublished data.
}

\references{
Warren, D., R.E. Glor, & M. Turelli. 2008. Environmental niche equivalency versus conservatism: quantitative approaches to niche evolution. \emph{Evolution}. \bold{62}: 2868-2883.
}

\examples{
# plot results of niche equivalency test
data(equ)
plotNicheEquivalency(equ)

# plot results of background similarity test
data(sim)
plotBackgroundSimilarity(sim)
}
\keyword{datasets}
