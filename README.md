# born

The born package provides new methods for the artificial addition of label noise in the borderline examples to add more challenging and yet realistic artificial noise to classification datasets. The main difference between the methods is the criterion and bias adopted to estimate which are the borderline examples to be disturbed: the first method is based on the ratio of intra/inter class Nearest Neighbor distance and the second method is based on the distance between the examples and the decision border induced by a radial kernel. 
  
## Installation

The installation process using devtools is:

```r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("lpfgarcia/born")
library("born")
```

## Example of use

The simplest way to generate the noisy dataset is using the `random`, `neighborwise` and `nonlinearwise` functions. The methods can be called by a symbolic description of the dataset (formula) or by a data frame (x, y). The parameters are the dataset and the ratio of noise. A simple example is given next:

```r
## Generate a noisy dataset based on random label noise
random(Species ~ ., iris, rate=0.1)

## Generate a noisy dataset based on nearest neighbor technique with 10% of noise
neighborwise(Species ~ ., iris, rate=0.1)

## Generate a noisy dataset based on non-linear technique with 10% of noise
nonlinearwise(Species ~ ., iris, rate=0.1)
```

## Developer notes

To cite `born` in publications use: Luis Garcia, Jens Lehmann, Andre de Carvalho, and Ana Lorena. (2018). born: Generate Borderline Noise for Classification Problems. R package version 0.1.0, https://github.com/lpfgarcia/born/

To submit bugs and feature requests, report at [project issues](https://github.com/lpfgarcia/born/issues).
