# forceR

**A vignette, which guides you through all functions of the package, is is available [here](https://htmlpreview.github.io/?https://github.com/Peter-T-Ruehr/forceR/blob/main/vignettes/forceR.html)**.

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

#### This is the development version of `forceR`
This GitHub repository contains the current development version of the R package `forceR`.

This development version may be more recent than the [official release of `forceR` on the Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/package=forceR) but could still include bugs.

You can install the development version of `forceR` from [GitHub](https://github.com/Peter-T-Ruehr/forceR):
```
require(devtools)
devtools::install_github("https://github.com/Peter-T-Ruehr/forceR")
```

#### Official release
To install the development version of `forceR` from [GitHub](https://github.com/Peter-T-Ruehr/forceR) use the following command (not yet valid):
```
install.packages('forceR')
```

### Citation
If you use this package, please cite the original publication (currently under review):

Rühr, PT & Blanke, A (**in press**): `forceX` and `forceR`: a mobile setup and R package to measure and analyze a wide range of animal closing forces. *Methods in Ecology and Evolution*.

An image of th eofficial pre-realease can be found at Zenodo:
[![DOI](https://zenodo.org/badge/469710038.svg)](https://zenodo.org/badge/latestdoi/469710038)
