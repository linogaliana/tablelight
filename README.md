
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tablelight

<!-- badges: start -->

[![pipeline
status](https://gitlab.com/linogaliana/texlight/badges/master/pipeline.svg)](https://gitlab.com/linogaliana/texlight/-/commits/master)
[![coverage
report](https://gitlab.com/linogaliana/texlight/badges/master/coverage.svg)](https://gitlab.com/linogaliana/texlight/-/commits/master)
<!-- badges: end -->

The goal of `tablelight` is to propose functions to generate regression
tables using as little as possible computer resources, especially RAM.
This package has been designed because `stargazer` requires a huge
amount of RAM to produce tables with voluminous data.

The basic idea is to `strip` regression objects from unnecessary fat and
use the lightened object to produce a regression table. The package
contains two sets of functions:

  - `strip` function: a set of methods to remove heavier elements from a
    regression objects ;
  - `light_table` function: a function to produce `LaTeX` tables (`HTML`
    tables in a future version).

The package is organized around a set of methods to extract information
from regression objects. The list of regression objects accepted is, for
the moment: `lm`, `glm`, `negbin`, `oglmx`, `zeroinfl`. Other types of
regression objects will be added in the future.

``` r
library(tablelight)
```

## Why do you need to strip fat from models ?

It is well known that regression objects are heavy in `R` (see this
[blog
post](http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/)
that inspired this package). For instance, the following regression
object is about 10 times heavier than the initial data:

``` r
df <- data.frame(
  x = rnorm(1e6),
  y = rnorm(1e6)
)
pryr::object_size(df)
#> 16 MB

regression <- lm(y ~ x, df)
pryr::object_size(regression)
#> 136 MB
```

Producing a `LaTeX` table with `stargazer` requires to call `summary`
that requires additional RAM:

``` r
library(profvis)
get_required_RAM <- function(profvis_object){
  return(
    max(profvis_object$x$message$prof$memalloc) - min(profvis_object$x$message$prof$memalloc)
  )
}
get_required_RAM(profvis(summary(regression)))
#> [1] 22.88872
```

To produce a regression table, that’s a deadly combo: you need to store
a heavy regression object in memory and need more memory to summarize it
in order to produce the table. With voluminous data, it is easy to make
your RAM hit the limit available. The idea behind `lighttable` is that
you just need heavy elements once (to produce the standard error
values). Once they have been used, heavy elements can be thrown away.

## A possible workflow

Let’s say you want to produce a regression table from two objects. For
the moment, the freedom in customizing the table is limited.

``` r
df <- data.frame(
  x = rnorm(1e6),
  y = rnorm(1e6)
)
df2 <- data.frame(
  x = rnorm(1e6),
  y = rnorm(1e6)
)

regression1 <- lm(y ~ x, df)
regression2 <- lm(y ~ x, df2)

get_required_RAM(profvis(
  capture.output(stargazer::stargazer(regression1, regression2)))
)
#> [1] 118.8371
```

With `tablelight`, you will :

1.  Strip the object from its fat with `strip` function ;
2.  Use `light_table` to produce the output. You should give objects as
    a list

<!-- end list -->

``` r
regression1 <- tablelight::strip(lm(y ~ x, df))
regression2 <- tablelight::strip(lm(y ~ x, df2))

get_required_RAM(profvis(
  capture.output(light_table(list(regression1, regression2))))
  )
#> [1] 0.6332245
```
