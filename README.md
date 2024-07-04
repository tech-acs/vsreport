# crvsreportpackage

## Overview

This package contains some useful functions to create a crvs report.

## Installation and Getting Started

To install and use the `crvsreportpackage` from GitHub, follow these steps to ensure all package dependencies are correctly managed and installed using renv:

1. Clone the repository:

```sh
git clone https://github.com/tech-acs/crvsreportpackage.git
cd crvsreportpackage
```

2. __Initialize the `renv` environment__: Upon first entering the package directory, `renv` will activate the specific package environment defined for `crvsreportpackage`. You need to run the following R commands to restore the required packages:

```R
install.packages("renv")
renv::restore()
```

This will read the `renv.lock` file which contains the dependencies and install the correct versions of all required packages.

3. __Install the package__: Once the environment is set up, you can install the package using:

```R
devtools::install()
```

By following these steps, you will ensure that you have all the necessary dependencies and the correct versions installed, allowing `crvsreportpackage` to function as intended.


## Usage

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

We are currently working on our documentation, this will soon be available in
website format.

## Contributing

We welcome contributions! We will shortly add Contributing Guidelines for details
on our code of conduct and the process for submitting pull requests.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Authors

- Pamela Kakande - Contributor and Maintainer - GitHub profile
- Tesfaye Belay - Contributor and Maintainer - [GitHub profile](https://github.com/tbelay)
- Rachel Shipsey - Contributor - GitHub profile
- Liam Beardsmore - Initial work - [GitHub profile](https://github.com/beardl-ons)
- Henry Partridge (ONS) - Contributor - [GitHub profile](https://github.com/rcatlord)
- Diego Lara (ONS) - Contributor - [GitHub profile](https://github.com/diego-ons)
