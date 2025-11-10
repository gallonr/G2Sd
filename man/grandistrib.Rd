\name{grandistrib}
\alias{grandistrib}
\title{
Composition of the sediment
}
\description{
This function provides a barplot of the different fractions composing the sediment
}
\usage{
grandistrib(x, main="", scale = "fine", xlab = "Stations", ylab = "Percentage")
}

\arguments{
  \item{x}{
A numeric matrix or data frame (see the shape of data(granulo))
}
  \item{main}{
a label for the title
}
  \item{scale}{
If fine, display the detailed composition; If large, display the simplify composition 
}
  \item{xlab}{
a label for the x axis, defaults to a description of x.
}
  \item{ylab}{
a label for the y axis, defaults to a description of y.
}
}
\details{
The obtained graph is commonly used by Sedimentologists
}
\value{
A barplot with the composition of sediment for each station sampled
}

\author{
Regis K. Gallon (CNAM-Intechmer)\cr
\email{regis.gallon@lecnam.net}
}

\seealso{
\code{\link[G2Sd]{granplot}}, \code{\link[G2Sd]{grandistrib}}
}
\examples{
data(granulo)
grandistrib(granulo, scale="fine")
}
