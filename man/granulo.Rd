\encoding{UTF8}
\name{granulo}
\alias{granulo}
\docType{data}
\title{
Data frame for G2Sd package
}
\description{
\kbd{granulo} is a data frame of 29 observations and 21 variables. The first column corresponds to the apertures sizes of AFNOR sieves, in micrometer (25000, 20000, 16000, 12500, 10000, 8000, 6300, 5000, 4000, 2500, 2000, 1600, 1250, 1000, 800, 630, 500, 400, 315, 250, 200, 160, 125, 100, 80, 63, 50, 40, 0).
Warning ! the last sieve 0 corresponds to the material retained in the < 40 micrometer pan after sieving.
The others columns corresponds to the weight of samples beside each size class
}
\usage{data(granulo)}
\format{
  A data frame with 29 rows corresponding to the apertures sizes on the following 21 stations sampled
  
}
\details{
This example provide a data frame of sedimentary data obtained with AFNOR sieves (in micrometer)
}
\source{
\cite{Godet, L., Fournier, J., Toupoint, N., Olivier, F. 2009. Mapping and monitoring intertidal benthic habitats: a review of techniques and proposal of a new visual methodology for the European coasts. \emph{Progress in Physical Geography} \strong{33}, 378-402}
}
\references{
\cite{Fournier, J., Godet, L., Bonnot-Courtois, C., Baltzer, A., Caline, B. 2009. Distribution des formations superficielles de l archipel de Chausey (Manche). \emph{Geologie de la France} \strong{1}, 5-17}
}
\examples{
data(granulo)
}