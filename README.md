# bivquant

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/bivquant)](https://CRAN.R-project.org/package=bivquant)

The goal of `bivquant` is to provide tools for bivariate discriminant analysis, focusing on the relationship between variable indicators. This package offers functions to analyze and visualize the interplay between two variables, aiding in the understanding of social sustainability metrics.

## Installation

You can install the development version of `bivquant` from [GitHub](https://github.com/RMKruse/bivarte-discriminant-package) with:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install bivquant package
devtools::install_github("RMKruse/bivarte-discriminant-package")
```



<!-- README.md is generated from README.Rmd. Please edit that file -->


## Example

Here’s a basic example demonstrating how to use the main function of the bivquant package:

``` r
# Load the bivquant package
library(bivquant)

# Load the example dataset
data("nutritionpoverty")

# Perform bivariate discriminant analysis
result <- bivqfun(data = nutritionpoverty, var1 = "NUval", var2 = "INval", tau = 0.25)

# View the result
print(result)
```

This example utilizes the nutritionpoverty dataset included in the package and applies the bivqfun function to analyze the relationship between the variables NUval (nutrition value) and INval (income value) with a specified threshold (tau) of 0.25.


<img src="man/figures/README-example-1.png" width="100%" />

## License

This project is licensed under the MIT License. See the LICENSE.md file for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request for any bugs, enhancements, or suggestions.

## Acknowledgments

Special thanks to all contributors and the open-source community for their invaluable support and resources.
