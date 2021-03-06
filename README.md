# G2Sd : Grain-size Statistics and Description of Sediment

## Description

G2Sd package gives full descriptive statistics and a physical description of sediment obtained with metric or phi sieves according to the grain size distribution.

## Details

The G2Sd package is an evolution of the Gradistat v.4.0 macro for MS Excel initially developped by Blott and Pye (2001) for phi sieves and Laser granulometer.
This package is suited to analyse data obtained from metric (micrometer) or phi sieves.
The user is required to input the weight of sediment retained on sieves spaced at any metric or phi intervals.
Statistics are calculated using arithmetic and geometric Method of Moments (micrometer) and using logarithmic Folk and Ward (1957) Method (phi scale): mean, standard-deviation, skewness, kurtosis.
The mode(s) is(are) determined graphically by the user (with a maximum of 4 modes). The determination of the mode is optional (no determination by default).
Several percentiles and common index are calculated: D10, D50, D90, D90/D10, D90-D10, D75/D25, D75-D25, Trask(So) Index, Krumbein(Qd) Index.
Physical description of texture, sorting, skewness or kurtosis are provided as such as the sediment name after Folk (1954).
Are also included the percentage of particules falling into each predefined size fraction, modified from Blott and Pye (2001) scale, Udden (1914) and Wentworth (1922).
There are three functions :

- `granstat` is a function which provides all results organized in two ways: a complete matrix (by default) or by separate items;
- `granplot` is a function which provides a histogramm with a cumulative percentage curve;
- `grandistrib` is a function which provides a barplot of the different fractions composing the sediment

## Authors

Regis K. Gallon (CNAM-Intechmer) [regis.gallon@lecnam.net](regis.gallon@lecnam.net), 
Jerome Fournier (CNRS) [fournier@mnhn.fr](fournier@mnhn.fr)

## References

Blott, S., Pye, K. 2001. Gradistat: grain size distribution and statistics package for the analysis of unconsolidated sediment. *Earth, Surface Processes and Landforms* **26**, 1237-1248

Folk, R.L. 1954. The distinction between grain size and mineral composition in sedimentary-rock nomenclature. *Journal of Geology* **62**, 344-359

Folk, R.L., Ward, W.C. 1957. Brazos River bar: a study in the significance of grain size parameters. *Journal of Sedimentary Petrology* **27**, 3-26

Krumbein, W.C., Pettijohn, F.J. 1938. *Manual of Sedimentary Petrography*. Appleton-Century-Crofts, New-York

Udden, J.A. 1914. Mechanical composition of clastic sediments. *Bulletin of the Geological Society of America* **25**, 655-744

Wentworth, C.K. 1922. A scale of grade and class terms for clastic sediments. *Journal of Geology* **30**, 377-392

## Example

```r
data(granulo)
data(coord_gran)
result=granstat(granulo)
granplot(granulo,1)
```