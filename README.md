
[![Travis build
status](https://travis-ci.org/moodymudskipper/doubt.svg?branch=master)](https://travis-ci.org/moodymudskipper/doubt)

# doubt

*doubt* overrides the operator `?` to provide:

  - **dubious operators** : Unary and binary operators from existing
    functions straight out of the box
  - **dubious pipes** : Piping operators of different precedences
  - **dubious syntaxes** : Flexible n-ary operators

The standard usages `?topic` and `type?topic` as documented in
`help("Question")` still work.

We refer to all of those as “dubious”, both as a reference to the
package name and to emphasize the fact that they’re not parsed as proper
operators.

### Installation

Install with :

``` r
remotes::install_github("moodymudskipper/doubt")
```

### Dubious operators

R provides some `%foo%` infix operators such as `%in%`, or `%/%`, their
precedence is between `^` and `*`.

Once the package *doubt* is attached, any accessible function can be
called as such operators and any precedence between and including `^`
and `<-` can be chosen. See examples below:

``` r
library(doubt, warn.conflicts = FALSE)

1:4 ^head? 2  + 1 # same as 1:head(4,2) + 1 
#> [1] 2 3 4 5
1:4 -head? 2  + 1 # same as head(1:4,2) + 1
#> [1] 2 3
1:4 %%head? 2 + 1 # same precedence as standard infix operators
#> [1] 2 3
1:4 ~head? 2  + 1 # low precedence, same as `~`
#> [1] 1 2 3
```

The precedence is given by the first symbol of the operator.

Unary operators can be used too starting by either `+`, `-`, `!` or `~`
:

``` r
~head? 1:10
#> [1] 1 2 3 4 5 6
```

It’s possible to pass more arguments by using `{}` on the rhs, this
works with both unary and binary forms and is interesting in that
arguments are separated by new lines (or `;`), and not by commas, which
can be handy in situations where commas can add confusions, such as
benchmarks, R6 definitions, or shiny UI functions.

``` r
"a" +paste? "b"
#> [1] "a b"
+paste?{"a"; "b"}
#> [1] "a b"
"a" +paste?{"b"; "c"}
#> [1] "a b c"

library(microbenchmark)
+microbenchmark?{
  a= sapply(iris, length)
  b= lengths(iris)
}
#> Unit: microseconds
#>  expr  min   lq   mean median    uq    max neval cld
#>     a 13.7 16.2 43.013  26.50 39.85 1334.6   100   a
#>     b 16.6 18.5 33.882  29.85 44.40   76.4   100   a

library(shiny)
cat(as.character(
  +fluidPage?{
    title = "Hello Shiny!"
    +fluidRow?{
      +column?{
        width = 4
        "4"
      }
      +column?{
        width = 3
        offset = 2
        "3 offset 2"
      }
    }
  }
))
#> <div class="container-fluid">
#>   <div class="row">
#>     <div class="col-sm-4">4</div>
#>     <div class="col-sm-3 col-sm-offset-2">3 offset 2</div>
#>   </div>
#> </div>

library(R6)
# example borrowed from https://adv-r.hadley.nz/r6.html and modified 
R6Class("Person", +list?{
  name = NULL
  age = NA
  initialize = function(name, age = NA) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(age), length(age) == 1)
    self$name <- name
    self$age <- age
  }
})
#> <Person> object generator
#>   Public:
#>     name: NULL
#>     age: NA
#>     initialize: function (name, age = NA) 
#>     clone: function (deep = FALSE) 
#>   Parent env: <environment: R_GlobalEnv>
#>   Locked objects: TRUE
#>   Locked class: FALSE
#>   Portable: TRUE
```

## Dubious pipes

Dubious pipes are similar to *magrittr*’s pipes, except we can choose
their precedence.

Piping with another precedence is useful when working interractively to
avoid editing brackets in many places. A common usecase is to avoid
brackets when calling `plotly::ggplotly()` on a `ggplot` object. The
calls below are indeed equivalent :

``` r
library(ggplot2)
library(plotly)
library(magrittr)
# standard use
ggplotly(ggplot(cars, aes(speed, dist)) + geom_point())
(ggplot(cars, aes(speed, dist)) + geom_point()) %>% ggplotly()
# using doubt
ggplot(cars, aes(speed, dist)) + geom_point() ~.? ggplotly(.)
```

### Dubious syntaxes

*doubt* supports the definition of complex syntaxes with very low
effort. This is better understood by an example :

``` r
`?add> {x} : {y}` <- function(x, y) { x + y}
?add> 2 : 3
#> [1] 5
```

For it to work the `?` symbol should either be used in its unary form at
the start of the expression as above, or in its binary form anywhere
else outside of `()`, `{}` and control flows. And of course, the
expression should be syntactic.

We can use it to define control flows :

``` r
`?FOR? {x} := {from} :step: {by} :to: {to} :do: {expr}` <- function(x, from, by, to, expr){
  eval.parent(substitute(
    for(x in seq(from, to, by)) expr
  ))
}
?FOR? z := 3 :step: 2 :to: 10 :do: print(z*10) 
#> [1] 30
#> [1] 50
#> [1] 70
#> [1] 90
```

### Registering a custom dubious op in a package

To be recognized by *doubt* a custom operator must either :

  - be defined in the global environment
  - be defined in a package AND registered

To define a dubious syntax in your package should import *doubt* and
reexport .

You can do so by adding this code to your package if you use *roxygen2*:

``` r
#' Modified question mark operator
#'
#' Reexported from package *doubt*
#' @inheritParams doubt::`?`
#' @export
`?` <- doubt::`?`
```

Additionally you should add an `.onAttach` function to your package
(usually done in a `zz.R` file) or edit yours so it updates *doubt*’s
options, as in the following :

``` r
.onAttach <- function(libname, pkgname) {
  doubt::register_dubious_syntaxes(c("?add> {x} : {y}", "?multiply> {x} : {y}"))
  invisible()
}
```
