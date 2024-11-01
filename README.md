# Introduction to `vsreport`

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview and Documentation

This package contains some useful functions to create a Civil Registration
Vital Statistics report.

The documentation for the package is available in website format on the link
that should be highlighted on the main page of the repository on the top right.

## Simple installation and Basic Usage

In order to use the `vsreport` as part of your pipeline with your data,
you will need to follow these straight forward commands:

### 1) Install and Load the `devtools` Package

Install and load the `devtools` package if you haven't already.
This will let you install packages from GitHub directly.

```r
install.packages("devtools")
library(devtools)
```

### 2) Install the `vsreport` Package from GitHub

Use the `install_github()` function from the `devtools` package to install.
You need to provide the repository address in the format username/repository.

```r
devtools::install_github("tech-acs/vsreport")
```

During this installation, you will be asked to install some required packages
so that the `vsreport` works correctly, this might take a few minutes.
You might get some errors if your `R` installation is old or there are some
necessary package updates, usually the messages on the Console are descriptive.

### 3) Use the `vsreport` Package in your Script

Once you have installed the package, here is a basic example of how to use
__vsreport__:

```r
library(vsreport)

# Example Usage
age_grp_80 <- construct_age_groups(ageinyrs, start_age = 5, max_band = 80,
                                        step_size = 5, under_1 = TRUE)
```

## Contributing

We welcome contributions!
Please have a look at our [Contributing Guidelines](CONTRIBUTING.md) for details
on the process for submitting pull requests.
Also have a read at our [Contributor Code of Conduct](CODE_OF_CONDUCT.md)
before interacting with the project.

## License

This project is licensed under the MIT License - see the [License](LICENSE.md)
file for more details on what you can and can't do with this code.

## Authors

- Pamela Kakande - Contributor and Maintainer
- Tesfaye Belay - Contributor and Maintainer - [GitHub profile](https://github.com/tbelay)
- Rachel Shipsey - Contributor
- Liam Beardsmore - Initial work - [GitHub profile](https://github.com/beardl-ons)
- Henry Partridge (ONS) - Contributor - [GitHub profile](https://github.com/rcatlord)
- Diego Lara (ONS) - Contributor - [GitHub profile](https://github.com/diego-ons)
