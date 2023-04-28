
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blueRsky

<!-- badges: start -->
<!-- badges: end -->

Talking to bluesky from R.

## Installation

You can install the development version of blueRsky like so:

``` r
devtools::install_github("colin-fraser/blueRsky")
```

You’ll also need my other package, `{wrapify}`, which you can install
with

``` r
devtools::install_github("colin-fraser/wrapify")
```

## Getting Started

It’s easy to start. Run `bsky_login()` with your username and an app
password.

``` r
library(blueRsky)
bsky_login(username, app_password)
```

Functions that start with `bsky_` talk to bluesky. Here’s how you can
send a post.

``` r
bsky_post("This post was created in R using the blueRsky package at github.com/colin-fraser/blueRsky")
```
