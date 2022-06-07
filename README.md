# forceR <img src="man/figures/logo.png" align="right" height="139" />

**A vignette which guides you through all functions of the package is available [here](https://htmlpreview.github.io/?https://github.com/Peter-T-Ruehr/forceR/blob/main/vignettes/forceR.html)**.

### Functionality
The package `forceR` has originally been written for insect bite force data preparation and analysis, but it can be used for any kind of time series measurements. Functions include 

* loading, plotting, and cropping of data
* correction of charge amplifier drifts
* correction of baseline drifts
* reduction of sampling frequency
* automatic extraction of single peaks
* rescaling (normalization) of curves
* reduction of curves to 100 time steps each
* finding of best polynomial fits to describe all curves

### Installation

#### Official release
To install the stable version of `forceR` from [CRAN](https://CRAN.R-project.org/package=forceR) use the following command:
```
install.packages('forceR')
```

#### Development version
You can also install the current development version of `forceR` from the [GitHub repository](https://github.com/Peter-T-Ruehr/forceR):
```
require(devtools)
devtools::install_github("https://github.com/Peter-T-Ruehr/forceR")
```


### Citation
If you use this package, please cite the original publication (currently under review):

Rühr, PT & Blanke, A (**2022**): forceX and `forceR`: a mobile setup and R package to measure and analyze a wide range of animal closing forces. *Methods in Ecology and Evolution*. doi: [10.1111/2041-210X.13909](https://doi.org/10.1111/2041-210X.13909).
