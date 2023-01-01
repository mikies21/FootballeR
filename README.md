
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FootballeR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of FootballeR is to streamline and condense all the football
match analysis into one single app without the need to lern or use R
directly. The data has been made freely available by
[StatsBomb](https://statsbomb.com/) using their
[StatsBombR](https://github.com/statsbomb/StatsBombR) package. The app
is built using Rshiny (with the
[golem](https://github.com/ThinkR-open/golem) framework) as a side
project to improve my shiny skills and combine it with my passion for
football and analysis!

The app is not hosted on shinyapps.io yet. soon..

## Installation

You can install the development version of FootballeR like so:

``` r
remotes::install_github("mikies21/FootballeR")
```

## Start the app

If you are familiar with the golem framework, the app IS essentially a
package.

1.  Once you Clone the Repository

2.  open the file **FootballeR.Rproj**

3.  navigate to the dev/run\_dev and run the pre-defined commands

The app should open and you should be able to browse all the free
leagues (hint: La Liga has seasons going back from 2010), pick the match
you would like to analyse and have fun.

## Development Areas

FootballeR shiny app only has the attacking section for its first
release, which focuses on shots. Due to my limited time, I will not be
looking at the defensive section anytime soon. Anyone can contribute to
this project and raise issues.
