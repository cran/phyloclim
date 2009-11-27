\name{palmatifoliae_pno}
\Rdversion{1.1}
\alias{palmatifoliae_pno}
\docType{data}
\title{
PNOs for Oxalis sect. Palmatifoliae
}
\description{
For 19 bioclimatic variables summarizing temperature and precipiation and five species of \emph{Oxalis}, the suitability or probability of occurence is given along the environmental gradients. 
}
\usage{data(palmatifoliae_pno)}
\format{
	
A list consisting of 19 matrixes. Each matrix corresponds to an environmental variable and contains the predicted niche occupancy profile (PNO) for each species of \emph{Oxalis} sect. \emph{Palmatifoliae}, whereby column 1 of each matrix gives the values of the environmental variable and columns 2-6 give probabilites of occurance of each species along the environmental gradient in column 1.

}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
C. Heibl, unpublished data.
}
\examples{
data(palmatifoliae_pno)
## a list of 19 bioclimatic variables:
names(palmatifoliae_pno)

## minimum temperature during coldest month
mintemp <- palmatifoliae_pno$MinTemperatureColdestMonth

## minimum temperate range occupied by Oxalis sect. Palmatifoliae
range(mintemp[, 1])

## plot PNO profile for minimum temperature
plotPNO(mintemp)
}
\keyword{datasets}
