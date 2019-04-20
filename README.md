
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R/`hal9001`

[![Travis-CI Build
Status](https://travis-ci.org/tlverse/hal9001.svg?branch=master)](https://travis-ci.org/tlverse/hal9001)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/jeremyrcoyle/hal9001?branch=master&svg=true)](https://ci.appveyor.com/project/jeremyrcoyle/hal9001)
[![Coverage
Status](https://img.shields.io/codecov/c/github/tlverse/hal9001/master.svg)](https://codecov.io/github/tlverse/hal9001?branch=master)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

> *Fast* and *scalable* estimation procedure for the Highly Adaptive
> LASSO

**Authors:** [Jeremy Coyle](https://github.com/tlverse) and [Nima
Hejazi](https://nimahejazi.org)

-----

## What’s `hal9001`?

`hal9001` is an R package providing an implementation of the scalable
Highly Adaptive LASSO (HAL), a nonparametric regression estimator that
applies L1-regularized regression (i.e., the LASSO) to a design matrix
composed of indicator functions corresponding to a set of covariates and
interactions thereof. Recent theoretical results show that HAL is
endowed with several important properties that make it optimally suited
for the purpose of estimating highly complex functions, including for
inference in problem settings where causal parameters are estimated via
data-adaptive techniques (i.e., machine learning), as is the case with
Targeted Minimum Loss-Based Estimation (TMLE). While it is certainly
possible to implement HAL purely in R, the computationally intensive
nature of the algorithm suggests that writing core routines in C++ (and
making these available in R via the [Rcpp](http://www.rcpp.org/)
framework) ought to provide significant efficiency gains. `hal9001` is
just such an implementation.

For detailed discussions of the Highly Adaptive LASSO estimator, the
interested reader might consider consulting Benkeser and van der Laan
(2016), van der Laan (2017a), and van der Laan (2017b).

-----

## Installation

<!--
For standard use, we recommend installing the package from
[CRAN](https://cran.r-project.org/) via


```r
install.packages("hal9001")
```
-->

You can install the development version of `hal9001` from GitHub via
[`devtools`](https://www.rstudio.com/products/rpackages/devtools/) with

``` r
devtools::install_github("tlverse/hal9001", build_vignettes = FALSE)
```

-----

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/tlverse/hal9001/issues).

-----

## Example

This minimal example shows how to use `hal9001` to obtain predictions
based on the Highly Adaptive LASSO. For details on the properties of the
estimator, the interested reader is referred to Benkeser and van der
Laan (2016) and van der Laan (2017a).

``` r
# load the hal9001 package
library(hal9001)
#> Loading required package: Rcpp
#> hal9001 v0.2.3: The Scalable Highly Adaptive Lasso

# simulate data
set.seed(385971)
n = 100
p = 3
x <- xmat <- matrix(rnorm(n * p), n, p)
y <- x[, 1] * sin(x[, 2]) + rnorm(n, mean = 0, sd = 0.2)

# fit the HAL regression
hal_fit <- fit_hal(X = x, Y = y)
#> [1] "Look Dave, I can see you're really upset about this. I honestly think you ought to sit down calmly, take a stress pill, and think things over."
hal_fit$times
#>                   user.self sys.self elapsed user.child sys.child
#> design_matrix         0.005    0.000   0.006          0         0
#> remove_duplicates     0.006    0.001   0.006          0         0
#> reduce_basis          0.000    0.000   0.000          0         0
#> lasso                 0.416    0.021   0.438          0         0
#> total                 0.427    0.022   0.450          0         0

# training sample prediction
preds <- predict(hal_fit, new_data = x)
mean(hal_mse <- (preds - y)^2)
#> [1] 0.008827644
```

-----

## Contributions

`hal9001` is the primary implementation of the Highly Adaptive LASSO, an
nonparametric function estimation procedure with numerous optimality
properties. While contributions are very welcome, we ask that interested
contributors consult our [contribution
guidelines](https://github.com/tlverse/hal9001/blob/master/CONTRIBUTING.md)
prior to submitting a pull request.

-----

## Citation

After using the `hal9001` R package, please cite the following:

``` 
    @misc{coyle2018hal9001,
      author = {Coyle, Jeremy R and Hejazi, Nima S},
      title = {{hal9001}: The Scalable {Highly Adaptive LASSO}},
      year  = {2018},
      howpublished = {\url{https://github.com/tlverse/hal9001}},
      url = {https://doi.org/DOI_TBD},
      doi = {DOI_TBD}
    }
```

-----

## License

© 2017-2018 [Jeremy R. Coyle](https://github.com/tlverse) & [Nima S.
Hejazi](https://nimahejazi.org)

The contents of this repository are distributed under the GPL-3 license.
See file `LICENSE` for details.

-----

## References

<div id="refs" class="references">

<div id="ref-benkeser2016hal">

Benkeser, David, and Mark J van der Laan. 2016. “The Highly Adaptive
Lasso Estimator.” In *2016 IEEE International Conference on Data Science
and Advanced Analytics (DSAA)*. IEEE.
<https://doi.org/10.1109/dsaa.2016.93>.

</div>

<div id="ref-vdl2017generally">

van der Laan, Mark J. 2017a. “A Generally Efficient Targeted Minimum
Loss Based Estimator Based on the Highly Adaptive Lasso.” *The
International Journal of Biostatistics*. De Gruyter.
<https://doi.org/10.1515/ijb-2015-0097>.

</div>

<div id="ref-vdl2017finite">

———. 2017b. “Finite Sample Inference for Targeted Learning.” *ArXiv
E-Prints*.

</div>

</div>
