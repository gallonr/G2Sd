\encoding{UTF8}
\name{granplot}
\alias{granplot}
\title{
Histogram with a cumulative percentage curve
}
\description{
This function provides a histogram of the grain-size distribution with a cumulative percentage curve
}
\usage{
granplot(x, xc = 1, hist = TRUE, cum = TRUE, main = "", 
			col.cum = "red", col.hist="gray", cexname=0.9,
			cexlab=1.3,decreasing=FALSE)
}
\arguments{
  \item{x}{
A numeric matrix or data frame (see the shape of data(granulo))
}
  \item{xc}{
A numeric value or a numeric vector to define columns
}
  \item{hist}{
If TRUE, display a histogram; if FALSE, do not display a histogram (only for only one column)
}
  \item{cum}{
If TRUE, display a cumulative percentage curve; if FALSE do not display a cumulative percentage curve (only for only one column)
}
  \item{main}{
Add a title to the current plot
}
  \item{col.cum}{
Color in which cumulative percentage curve will be drawn
}
\item{col.hist}{
Color in which histogram will be drawn
}
\item{cexname}{
A numerical value giving the amount by which plotting text and symbols should be magnified
relative to the default.
}
\item{cexlab}{
A numerical value giving the amount by which axis labels should be magnified
relative to the default.
}
\item{decreasing}{
A logical value defining the order increasing or decreasing
}
}
\details{
The obtained graph is the most commonly used by Sedimentologists
}
\value{
A histogram with a cumulative percentage curve
}
\author{
Regis K. Gallon (MNHN) \email{reg.gallon@gmail.com}, 
Jerome Fournier (CNRS) \email{fournier@mnhn.fr}
}
\seealso{
\code{\link[G2Sd]{grandistrib}}
}
\examples{
data(granulo)
granplot(granulo,xc=1,hist=TRUE,cum=TRUE,main="Grain-size Distribution",
			col.hist="gray",col.cum="red")
      
granplot(granulo,xc=2:4,main="Grain-size Distribution")
}