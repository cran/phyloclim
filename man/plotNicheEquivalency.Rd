\name{plotNicheEquivalency}
\Rdversion{1.1}
\alias{plotNicheEquivalency}
\alias{plotBackgroundSimilarity}

\title{
Visualize results of niche equivalency and background similarity tests.
}

\description{
This functions plot the estimates of niche overlap D and I together with their expected null distribution based on randomization.
}

\usage{
plotNicheEquivalency(x)

plotBackgroundSimilarity(x)
}


\arguments{
  \item{x}{A list with five elements: (1) type of test, (2) species names, (3) D, (4) I, and (5) null distributions as produced by \code{\link{niche.equivalency.test}} and \code{\link{bg.similarity.test}}.
}
}

\author{
Christoph Heibl (\email{heibl@lmu.de})
}

\seealso{
\code{\link{niche.equivalency.test}}, 
\code{\link{bg.similarity.test}},
\code{\link{niche.overlap}}
}
\examples{
# plot results of niche equivalency test
data(equ)
plotNicheEquivalency(equ)

# plot results of background similarity test
data(sim)
plotBackgroundSimilarity(sim)

# These results suggest that O. adenophylla and O. arenaria
# differ significantly in their climatic niches, but their niches 
# are neither more nor less similar than expacted by chance. 
}

