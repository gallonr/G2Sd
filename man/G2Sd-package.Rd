\encoding{UTF8}
\name{G2Sd}
\Rdversion{2.0}
\alias{G2Sd-package}
\alias{G2Sd}
\docType{package}
\title{
Grain-size Statistics and Description of Sediment
}
\description{
G2Sd package gives full descriptive statistics and a physical description of sediment obtained with metric or phi sieves according to the grain size distribution.
}
\details{
The G2Sd package is an evolution of the Gradistat v.4.0 macro for MS Excel initially developped by Blott and Pye (2001) for phi sieves and Laser granulometer.
This package is suited to analyse data obtained from metric (micrometer) or phi sieves.
The user is required to input the weight of sediment retained on sieves spaced at any metric or phi intervals.
Statistics are calculated using arithmetic and geometric Method of Moments (micrometer) and using logarithmic Folk and Ward (1957) Method (phi scale): mean, standard-deviation, skewness, kurtosis.
The mode(s) is(are) determined graphically by the user (with a maximum of 4 modes). The determination of the mode is optional (no determination by default).
Several percentiles and common index are calculated: D10, D50, D90, D90/D10, D90-D10, D75/D25, D75-D25, Trask(So) Index, Krumbein(Qd) Index.
Physical description of texture, sorting, skewness or kurtosis are provided as such as the sediment name after Folk (1954).
Are also included the percentage of particules falling into each predefined size fraction, modified from Blott and Pye (2001) scale, Udden (1914) and Wentworth (1922).
There are four functions. \kbd{granstat} is a function which provides all results organized in two ways: a complete matrix (by default) or by separate items;
\kbd{granplot} is a function which provides a histogramm with a cumulative percentage curve;
\kbd{grandistrib} is a function which provides a barplot of the different fractions composing the sediment;
\kbd{granmap} is a function which provides a georeferenced map of the sediment distribution.
}
\author{
Regis K. Gallon (CNAM-Intechmer) \email{regis.gallon@lecnam.net}, 
Jerome Fournier (CNRS) \email{fournier@mnhn.fr}
}
\references{
\cite{Blott, S., Pye, K. 2001. Gradistat: grain size distribution and statistics package for the analysis of unconsolidated sediment. \emph{Earth, Surface Processes and Landforms} \strong{26}, 1237-1248}

\cite{Folk, R.L. 1954. The distinction between grain size and mineral composition in sedimentary-rock nomenclature. \emph{Journal of Geology} \strong{62}, 344-359}

\cite{Folk, R.L., Ward, W.C. 1957. Brazos River bar: a study in the significance of grain size parameters. \emph{Journal of Sedimentary Petrology} \strong{27}, 3-26}

\cite{Krumbein, W.C., Pettijohn, F.J. 1938. \emph{Manual of Sedimentary Petrography}. Appleton-Century-Crofts, New-York}

\cite{Udden, J.A. 1914. Mechanical composition of clastic sediments. \emph{Bulletin of the Geological Society of America} \strong{25}, 655-744}

\cite{Wentworth, C.K. 1922. A scale of grade and class terms for clastic sediments. \emph{Journal of Geology} \strong{30}, 377-392}
}
\seealso{
\code{\link[G2Sd]{granstat}}, \code{\link[G2Sd]{granplot}}, \code{\link[G2Sd]{grandistrib}}
}
\examples{
data(granulo)
data(coord_gran)
result=granstat(granulo)
granplot(granulo,1)
}