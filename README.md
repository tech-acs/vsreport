# crvsreportpackage

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

This package contains some useful functions to create a crvs report.

## Simple installation and Basic Usage

In order to use the `crvsreportpackage` with your data, you will need to follow these straight forward commands.
First, install the package from the GitHub repo:

### Install and Load the devtools Package

Install and load the devtools package if you haven't already.

```r
install.packages("devtools")
library(devtools)
```

### Install the `crvsreportpackage` Package from GitHub

Use the install_github() function from the devtools package to install the package.
You need to provide the repository address in the format username/repository.

```r
devtools::install_github("tech-acs/crvsreportpackage")
```

During this installation, you will be asked to install some required packages that are used within `crvsreportpackage`.
This might take a minute or two.

### Use the `crvsreportpackage` in your Script

Once you have installed the package, here is a basic example of how to use
__crvsreportpackage__:

```r
library(crvspackage)

# Example Usage
age_grp_80 <- derive_age_groups(ageinyrs,
                                        start_age = 5, max_band = 80,
                                        step_size = 5, under_1 = TRUE)
```

## Documentation

The documentation for the package is available in the website link that should be highlighted on the main page of the repository on the top right.

## Contributing

We welcome contributions! Please have a look at our [Contributing Guidelines](CONTRIBUTING.md) for details
on the process for submitting pull requests. Also have a read at our [Contributor Code of Conduct](CODE_OF_CONDUCT.md) before interacting with the project.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Authors

- Pamela Kakande - Contributor and Maintainer - GitHub profile
- Tesfaye Belay - Contributor and Maintainer - [GitHub profile](https://github.com/tbelay)
- Rachel Shipsey - Contributor - GitHub profile
- Liam Beardsmore - Initial work - [GitHub profile](https://github.com/beardl-ons)
- Henry Partridge (ONS) - Contributor - [GitHub profile](https://github.com/rcatlord)
- Diego Lara (ONS) - Contributor - [GitHub profile](https://github.com/diego-ons)
