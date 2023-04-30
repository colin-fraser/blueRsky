
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

Functions that start with `bsky_` talk to bluesky.

## Sending a post

Here’s how you can send a post.

``` r
post <- bsky_create_post("This post was created in R using the blueRsky package at github.com/colin-fraser/blueRsky")
post
#> $uri
#> [1] "at://did:plc:6rdthm3ihpqfd7vn2q2ktjrz/app.bsky.feed.post/3jumdeihsxd2c"
#> 
#> $cid
#> [1] "bafyreiddaxssofhknn4eu5q73tafnzfty73t42yipnxfj36ucwfaodqo6m"
```

## Fetch a timeline

``` r
tl <- bsky_get_timeline(limit = 10)
tl$feed |> 
  purrr::map(\(x) {
    paste(x$post$author$handle, x$post$record$text, sep = ': ')
  })
#> [[1]]
#> [1] "colin-fraser.net: This post was created in R using the blueRsky package at github.com/colin-fraser/blueRsky"
#> 
#> [[2]]
#> [1] "lookitup.baby: also I told my kid I would help him make a DHMIS bot"
#> 
#> [[3]]
#> [1] "birb.bsky.social: personally i believe animals can feel when we’re having bad days and do their best to fix them for us"
#> 
#> [[4]]
#> [1] "saeeddicaprio.bsky.social: this app has the opportunity to do the coolest thing ever (put the trans flag beside everyone’s name so transphobes don’t ruin another app)"
#> 
#> [[5]]
#> [1] "lookitup.baby: we don't have /meow yet"
#> 
#> [[6]]
#> [1] "lookitup.baby: is the honk bot coming back? if not I want to make one"
#> 
#> [[7]]
#> [1] "jay.bsky.team: i don’t know what any of you are talking about, it’s obviously a white dress"
#> 
#> [[8]]
#> [1] "why.bsky.team: as a pds operator? not really. technically you could rewrite all amazon links posted through your PDS to be affiliate links, but that would likely break trust and cause your users to migrate away"
#> 
#> [[9]]
#> [1] "birb.bsky.social: it’s nice to know there are other people that understand it. i’m sorry that you do! i wish it was entirely unrelatable, and i don’t think we’ve done anything to deserve it. ❤️"
#> 
#> [[10]]
#> [1] "taylorlorenz.bsky.social: If you don’t support my website ur an Obama birther is certainly a take"
```
