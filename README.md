
# CombineDistributions

<!-- badges: start -->
<!-- badges: end -->

This package implements multiple methods for combining probabilistic predictions, including probability and quantile averaging, i.e.,   Linear Opinion Pool (Stone 1961) and Vincent average (Vincent 1912, Ratcliff 1979) respectively, as well as non-equal weighting, including trimming methods (Jose 2014). 


All code is licensed under the CC-BY-NC Creative Commons attribution-noncommercial license (http://creativecommons.org/licenses/by-nc/3.0/). This package can be referenced by citing 

<center>Howerton et al. Submitted. </center>


## Installation

You can install the released version of CombineDistributions from GitHub with:

``` r
remotes::install_github("eahowerton/CombineDistributions")
```
## Vignettes
Please reference our vignettes to see examples of the functionality in `CombineDistributions`. 

1. Introduction to `CombineDistributions`: this vignette provides an overview of the aggregation methods available and how to implement them.

2. Aggregating predictions from an SIRS mdoel: this vignette illustrates a simple simulation case study, where we simulate a multi-model prediction effort and compare aggregation methods in this controlled environment. 

3. Aggregation and trimming on real-world COVID-19 death predictions: this vignette aggregates projections on COVID-19 deaths from 17 distinct epidemiological models, including varying the aggregation approach and the weighting scheme.

## References
Jose, Victor Richmond R., Yael Grushka-Cockayne, and Kenneth C. Lichtendahl. 2014. “Trimmed Opinion Pools and the Crowd’s Calibration Problem.” Management Science 60 (2): 463–75. https://doi.org/10.1287/mnsc.2013.1781.

Ratcliff, Roger. 1979. “Group Reaction Time Distributions and an Analysis of Distribution Statistics.” Psychological Bulletin 86 (3): 446–61. https://doi.org/10.1037/0033-2909.86.3.446.


Stone, M. 1961. “The Opinion Pool.” The Annals of Mathematical Statistics 32 (4): 1339–42.

Vincent, Stella Burnham. 1912. “The Function of the Vibrissae in the Behavior of the White Rat.” Cambridge MA: Holt.




