\encoding{UTF8}
\name{grantern}
\alias{grantern}
\title{
Ternary diagram for sediment composition
}
\description{
This function provides a ternary diagram (Folk triangle) showing the relative proportions
of Gravel, Sand, and Mud in sediment samples
}
\usage{
grantern(x, main = "Ternary Diagram", show_labels = TRUE,
         point_size = 3, point_color = "steelblue", show_grid = TRUE)
}

\arguments{
  \item{x}{
A numeric matrix or data frame (see the shape of data(granulo))
}
  \item{main}{
A label for the title of the plot
}
  \item{show_labels}{
A logical value; if TRUE (default), display sample labels on the plot;
if FALSE, display only points
}
  \item{point_size}{
A numeric value specifying the size of the points (default: 3)
}
  \item{point_color}{
A character string specifying the color of the points (default: "steelblue")
}
  \item{show_grid}{
A logical value; if TRUE (default), display a ternary grid with 10\% intervals;
if FALSE, display only the triangle outline
}
}

\details{
The ternary diagram (also known as the Folk triangle) is a standard tool in sedimentology
for visualizing the relative proportions of three sediment fractions: Gravel (>2mm),
Sand (63µm - 2mm), and Mud (<63µm). Each corner of the triangle represents 100\% of
one component, and points are plotted based on their relative percentages.

The function uses ggplot2 to create the diagram, with proper coordinate transformation
from ternary to Cartesian coordinates. A grid is optionally displayed to facilitate
reading of percentages.

The sediment texture is automatically calculated from the grain-size distribution data
using the internal function \code{.texture.sedim}.
}

\value{
Displays a ternary plot and invisibly returns a data frame containing:
\item{samples}{Sample names}
\item{Gravel}{Percentage of gravel (>2mm)}
\item{Sand}{Percentage of sand (63µm - 2mm)}
\item{Mud}{Percentage of mud (<63µm)}
\item{x}{Cartesian x-coordinate for plotting}
\item{y}{Cartesian y-coordinate for plotting}
}

\author{
Regis K. Gallon (CNAM-Intechmer)\cr
\email{regis.gallon@lecnam.net}\cr
Jerome Fournier (CNRS)\cr
\email{fournier@mnhn.fr}
}

\references{
Folk, R.L. (1954). The distinction between grain size and mineral composition
in sedimentary-rock nomenclature. Journal of Geology, 62(4), 344-359.
}

\seealso{
\code{\link[G2Sd]{granplot}}, \code{\link[G2Sd]{grandistrib}}, \code{\link[G2Sd]{granstat}}
}

\examples{
# Load example data
data(granulo)

# Basic ternary diagram with grid and labels
grantern(granulo, main = "Folk Triangle - Sediment Composition")

# Ternary diagram without sample labels
grantern(granulo, main = "Folk Triangle", show_labels = FALSE)

# Customized diagram with larger red points and no grid
grantern(granulo, main = "Sediment Texture",
         point_size = 5, point_color = "darkred", show_grid = FALSE)

# Minimal diagram
grantern(granulo, show_labels = FALSE, show_grid = FALSE)
}
