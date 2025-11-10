# G2Sd : Grain-size Statistics and Description of Sediment

<img src="data/G2SdLogo.png" align="right" width="200"/>


[![CRAN status](https://www.r-pkg.org/badges/version/G2Sd)](https://CRAN.R-project.org/package=G2Sd) [![Downloads](https://cranlogs.r-pkg.org/badges/G2Sd)](https://cran.r-project.org/package=G2Sd) [![R version](https://img.shields.io/badge/R-%E2%89%A5%203.0.2-blue)](https://www.r-project.org/) [![Language](https://img.shields.io/badge/language-R-blue.svg)](https://www.r-project.org/)


## Authors

-   Regis K. Gallon (CNAM-Intechmer) [regis.gallon\@lecnam.net](regis.gallon@lecnam.net)
-   Jerome Fournier (CNRS) [fournier\@mnhn.fr](fournier@mnhn.fr)

## Overview

**G2Sd** provides comprehensive descriptive statistics and physical descriptions of sediment based on grain-size distribution data obtained from metric or phi sieves.

## Details

The G2Sd package is an evolution of the Gradistat v.4.0 macro for MS Excel initially developped by Blott and Pye (2001) for phi sieves and Laser granulometer. This package is suited to analyse data obtained from metric (micrometer) or phi sieves. The user is required to input the weight of sediment retained on sieves spaced at any metric or phi intervals. Statistics are calculated using arithmetic and geometric Method of Moments (micrometer) and using logarithmic Folk and Ward (1957) Method (phi scale): mean, standard-deviation, skewness, kurtosis. The mode(s) is(are) determined graphically by the user (with a maximum of 4 modes). The determination of the mode is optional (no determination by default). Several percentiles and common index are calculated: D10, D50, D90, D90/D10, D90-D10, D75/D25, D75-D25, Trask(So) Index, Krumbein(Qd) Index. Physical description of texture, sorting, skewness or kurtosis are provided as such as the sediment name after Folk (1954). Are also included the percentage of particules falling into each predefined size fraction, modified from Blott and Pye (2001) scale, Udden (1914) and Wentworth (1922). There are three functions :

-   `granstat` is a function which provides all results organized in two ways: a complete matrix (by default) or by separate items;
-   `granplot` is a function which provides a histogramm with a cumulative percentage curve;
-   `grandistrib` is a function which provides a barplot of the different fractions composing the sediment

## Installation

### From CRAN (Stable Version)

The easiest way to install G2Sd is from CRAN:

``` r
install.packages("G2Sd")
```

### From GitHub (Development Version)

To install the development version from GitHub:

``` r
# Install remotes package 
install.packages("remotes")

# Install G2Sd from GitHub
remotes::install_github("gallonr/G2Sd")
```

After installation, load the package:

``` r
library(G2Sd)
```

## Quick Start Examples

### Loading the Package and Data

``` r
library(G2Sd)

# Load example dataset
data(granulo)

# View the structure of the data
head(granulo)
```

The `granulo` dataset contains grain-size data from 21 sediment samples with AFNOR sieve sizes (in micrometers).

### Example 1: Basic Statistical Analysis

Calculate grain-size statistics for a single sample:

``` r
# Analyze the first sample (column 2)
result <- granstat(granulo, sample = 2)

# View the complete results
print(result)
```

This will provide: - Mean, median, standard deviation, skewness, and kurtosis - Key percentiles (D10, D50, D90) - Sediment classification (Folk 1954) - Textural descriptions

### Example 2: Visualizing Grain-Size Distribution

Create a histogram with cumulative curve:

``` r
# Plot the grain-size distribution for sample 1
granplot(granulo, sample = 1)

# Plot with custom title
granplot(granulo, sample = 1, main = "Station A - Grain Size Distribution")
```

### Example 3: Analyzing Sediment Fractions

Display the distribution of sediment fractions:

``` r
# Show sediment fractions for sample 5
grandistrib(granulo, sample = 5)

# This creates a barplot showing percentages of:
# - Gravel, sand (very coarse to very fine), silt, and clay fractions
```

### Example 4: Batch Processing Multiple Samples

Analyze all samples in the dataset:

``` r
# Create a loop to analyze all samples
results_list <- list()
for (i in 2:ncol(granulo)) {
  results_list[[i-1]] <- granstat(granulo, sample = i)
}

# Extract mean grain sizes for all samples
mean_sizes <- sapply(2:ncol(granulo), function(i) {
  res <- granstat(granulo, sample = i)
  res$mmean  # Mean grain size in micrometers
})

print(mean_sizes)
```

### Example 5: Complete Analysis Workflow

``` r
library(G2Sd)

# Load data
data(granulo)

# Analyze sample 3
sample_stats <- granstat(granulo, sample = 3)

# Display key statistics
cat("Mean grain size:", sample_stats$mmean, "µm\n")
cat("Sorting:", sample_stats$sorting_descr, "\n")
cat("Sediment name:", sample_stats$sediment_name, "\n")

# Visualize the distribution
granplot(granulo, sample = 3)

# Show fraction composition
grandistrib(granulo, sample = 3)
```

## Getting Help

For detailed documentation on each function:

``` r
?granstat      # Statistical analysis
?granplot      # Histogram and cumulative curve
?grandistrib   # Fraction distribution
?granulo       # Example dataset description
```

## Citation

If you use G2Sd in your research, please cite:

``` r
citation("G2Sd")
```

## Contributing

Contributions, bug reports, and feature requests are welcome! Please feel free to open an issue or submit a pull request on [GitHub](https://github.com/gallonr/G2Sd).

## References

Blott, S., Pye, K. 2001. Gradistat: grain size distribution and statistics package for the analysis of unconsolidated sediment. *Earth, Surface Processes and Landforms* **26**, 1237-1248

Folk, R.L. 1954. The distinction between grain size and mineral composition in sedimentary-rock nomenclature. *Journal of Geology* **62**, 344-359

Folk, R.L., Ward, W.C. 1957. Brazos River bar: a study in the significance of grain size parameters. *Journal of Sedimentary Petrology* **27**, 3-26

Krumbein, W.C., Pettijohn, F.J. 1938. *Manual of Sedimentary Petrography*. Appleton-Century-Crofts, New-York

Udden, J.A. 1914. Mechanical composition of clastic sediments. *Bulletin of the Geological Society of America* **25**, 655-744

Wentworth, C.K. 1922. A scale of grade and class terms for clastic sediments. *Journal of Geology* **30**, 377-392

## License

GPL-3

------------------------------------------------------------------------

**Maintainer:** Regis K. Gallon (CNAM-Intechmer) - [regis.gallon\@lecnam.net](mailto:regis.gallon@lecnam.net)