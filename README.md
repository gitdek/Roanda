# Roanda


Roanda is a new package that makes it incredibly easy to integrate with OANDA API.


[![Build Status] (https://travis-ci.org/gitdek/Roanda.svg?branch=master)](https://travis-ci.org/gitdek/Roanda)


## Features

* Interactive web UI using the [shiny](https://github.com/rstudio/shiny) package for monitoring and debugging.
* Works in any R environment (Console R, Rgui for Windows or Mac, ESS, StatET, RStudio, etc.).

## Installation

To install the stable version from CRAN, simply run the following from an R console:

```r
install.packages("Roanda")
```

To install the latest development builds directly from GitHub, run this instead:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("gitdek/Roanda")
```

## Getting Started
To learn more we highly recommend you check out the [Roanda Tutorial](https://gitdek.github.io/Roanda). The tutorial explains the framework in-depth, walks you through building a simple application, and includes extensive annotated examples.

We hope you enjoy using Roanda. For bug reports, please use the [issue tracker](https://github.com/gitdek/Roanda/issues).
