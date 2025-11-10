\encoding{UTF8}
\name{granstat}
\alias{granstat}
\title{
Calculates all descriptive statistics
}
\description{
Statistics are calculated using arithmetic and geometric Method of Moments (micrometer) and using logarithmic Folk and Ward (1957) Method (phi scale): mean, standard-deviation, skewness, kurtosis.
The mode(s) is(are) determined graphically by the user (with a maximum of 4 modes). The determination of the mode is optional (no determination by default).
Several percentiles and common index are calculated: D10, D50, D90, D90/D10, D90-D10, D75/D25, D75-D25, Trask(So) Index, Krumbein(Qd) Index.
Physical description of texture, sorting, skewness or kurtosis are provided as such as the sediment name after Folk (1954).
Are also included the percentage of particules falling into each predefined size fraction, modified from Blott and Pye (2001) scale, Udden (1914) and Wentworth (1922).
\kbd{granstat} is a function which provides all results organized in two ways: a complete matrix (by default) or by separate items.
}
\usage{
granstat(x, phiSize=FALSE, web_interface=FALSE, statistic = "all", modes = FALSE,
FromLargetoSmall=TRUE)
}
\arguments{
  \item{x}{
A numeric matrix or data frame
}
  \item{phiSize}{
If TRUE, phi meshsizes are used . If FALSE, micrometer meshsizes are used 
}
  \item{web_interface}{
if TRUE, a simplified interface is displayed from your default web browser
}
  \item{statistic}{
Statistic used: "arithmetic", "geometric", "folk.ward", "all". If this argument is not used, all statistics are calculated
}

  \item{modes}{
If TRUE, the mode must be determinated graphically by the user. If FALSE, the mode is not determinated. If this argument is not used, no determination of the mode is proposed
}
  \item{FromLargetoSmall}{
If TRUE, sieves are odered from the larger to the smaller . If FALSE,
sieves are odered from the smaller to the larger.
}

}
\details{
For the determination of the mode (\kbd{modes=TRUE}).  All the samples are successively shown with a graph.
The user can choose graphically the mode (1 in 4 maximum) by a click on the graph.
If 4 modes are chosen, the following graph appears automatically.
If 1, 2 or 3 modes are chosen, the user has to use the function stop locator in the graphic window.

If the weight of sediment retained on the broadest sieve exceeds 5 percent of the total mass of the sample, the Folk and Ward statistics cannot be computed. 
}
\value{
A list containing

\item{mean.arith}{the mean of grain-size distribution (arithmetic method of moments)}
\item{sd.arith}{the standard-deviation of grain-size distribution (arithmetic method of moments)}
\item{skewness.arith}{the skewness of grain-size distribution (arithmetic method of moments)}
\item{kurtosis.arith}{the kurtosis of grain-size distribution (arithmetic method of moments)}
\item{mean.geom}{the mean of grain-size distribution (geometric method of moments}
\item{sd.geom}{the standard-deviation of grain-size distribution (geometric method of moments)}
\item{skewness.geom}{the skewness of grain-size distribution (geometric method of moments)}
\item{kurtosis.geom}{the kurtosis of grain-size distribution (geometric method of moments)}
\item{Sediment}{physical description of the sediment, the sorting, the skewness and the kurtosis}
\item{Mean.fw.mm}{the mean of grain-size distribution (logarithmic Folk and Ward method, mm scale)}
\item{Sd.fw.mm}{the standard-deviation of grain-size distribution (logarithmic Folk and Ward method, mm scale)}
\item{Skewness.fw.mm}{the skewness of grain-size distribution (logarithmic Folk and Ward method, mm scale)}
\item{Kurtosis.fw.mm}{the kurtosis of grain-size distribution (logarithmic Folk and Ward method, mm scale)}                          
\item{Mean.fw.phi}{the mean of grain-size distribution (logarithmic Folk and Ward method, phi scale)}                                 
\item{Sd.fw.phi}{the standard-deviation of grain-size distribution (logarithmic Folk and Ward method, phi scale)}                                       
\item{Skewness.fw.phi}{the skewness of grain-size distribution (logarithmic Folk and Ward method, phi scale)}                          
\item{Kurtosis.fw.phi}{the kurtosis of grain-size distribution (logarithmic Folk and Ward method, phi scale)}                                   
\item{Mode}{the mode (mm scale), graphically defined by the user}                                                       
\item{D10(mm)}{the 10th percentile}             
\item{D50(mm)}{the median}                                        
\item{D90(mm)}{the 90th percentile}
\item{D90/D10}{ratio of the 90th percentile and the 10th percentile}                                     
\item{D90-D10}{difference between the the 90th percentile and the 10th percentile}                                     
\item{D75/D25}{ratio of the 75th percentile and the 25th percentile}                                          
\item{D75-D25}{difference between the the 75th percentile and the 25th percentile}                                        
\item{Trask(So)}{the Trask Index (So) defined as D25/D75 (mm scale)}                                        
\item{Krumbein(Qd)}{the Krumbein Index (Qd) defined as (D25-D75)/2 (phi scale)}                                            
\item{Texture}{physical description of the texture of the sediment}
\item{Boulder}{percentage of sediment of the grain-size distribution retained in the Boulder class (upper to 63 mm)}                                                
\item{Gravel}{percentage of sediment of the grain-size distribution retained in the Gravel class (between 2 mm and 63 mm)}                                           
\item{Sand}{percentage of sediment of the grain-size distribution retained in the Sand class (between 63 micrometer and 2 mm)}                                          
\item{Mud}{percentage of sediment of the grain-size distribution retained in the Mud class (down to 63 micrometer)}                                       
\item{Boulder}{percentage of sediment of the grain-size distribution retained in the Boulder class (upper to 63 mm)}                                                                                                                    
\item{vcgravel}{percentage of sediment of the grain-size distribution retained in the Very Coarse Gravel class (between 31.5 mm and 63 mm)}                                                             
\item{cgravel}{percentage of sediment of the grain-size distribution retained in the Coarse Gravel class (between 16 mm and 31.5 mm)}                                                             
\item{mgravel}{percentage of sediment of the grain-size distribution retained in the Medium Gravel class (between 8 mm and 16 mm)}                                                             
\item{fgravel}{percentage of sediment of the grain-size distribution retained in the Fine Gravel class (between 4 mm and 8 mm)}                                                             
\item{vfgravel}{percentage of sediment of the grain-size distribution retained in the Very Fine Gravel class (between 2 mm and 4 mm)}                                                             
\item{vcsand}{percentage of sediment of the grain-size distribution retained in the Very Coarse Sand class (between 1 mm and 2 mm)}                                                             
\item{csand}{percentage of sediment of the grain-size distribution retained in the Coarse Sand class (between 500 micrometer and 1 mm)}                                              
\item{msand}{percentage of sediment of the grain-size distribution retained in the Medium Sand class (between 250 micrometer and 500 micrometer)}                                             
\item{fsand}{percentage of sediment of the grain-size distribution retained in the Fine Sand class (between 125 micrometer and 250 micrometer)}                                            
\item{vfsand}{percentage of sediment of the grain-size distribution retained in the Very Fine Sand class (between 63 micrometer and 125 micrometer)}                                                             
\item{vcsilt}{percentage of sediment of the grain-size distribution retained in the Very Coarse Silt class (between 40 micrometer and 63 micrometer)}                                              
\item{silt}{percentage of sediment of the grain-size distribution retained in the Silt class (lower than 40 micrometer)}      
}
\references{
\cite{Blott, S., Pye, K. 2001. Gradistat: grain size distribution and statistics package for the analysis of unconsolidated sediment. \emph{Earth, Surface Processes and Landforms} \strong{26}, 1237-1248}

\cite{Folk, R.L. 1954. The distinction between grain size and mineral composition in sedimentary-rock nomenclature. \emph{Journal of Geology} \strong{62}, 344-359}

\cite{Folk, R.L., Ward, W.C. 1957. Brazos River bar: a study in the significance of grain size parameters. \emph{Journal of Sedimentary Petrology} \strong{27}, 3-26}

\cite{Krumbein, W.C., Pettijohn, F.J. 1938. \emph{Manual of Sedimentary Petrography}. Appleton-Century-Crofts, New-York}

\cite{Udden, J.A. 1914. Mechanical composition of clastic sediments. \emph{Bulletin of the Geological Society of America} \strong{25}, 655-744}

\cite{Wentworth, C.K. 1922. A scale of grade and class terms for clastic sediments. \emph{Journal of Geology} \strong{30}, 377-392}
}
\author{
Regis K. Gallon (CNAM-Intechmer) \email{regis.gallon@lecnam.net},
Jerome Fournier (CNRS) \email{fournier@mnhn.fr}
}

\examples{
#granulo is the data set
data(granulo)
granstat(granulo)
granstat(granulo,statistic="all",modes=FALSE)
granstat(granulo,statistic="folk.ward",modes=TRUE)

#to display the simplified interface
#granstat(web_interface=TRUE)
}