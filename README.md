
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doubt

*doubt* modifies the operator `?` to allow definition of new operators
(unary, binary or n-ary). We refer to those as “dubious” operators, both
as a reference to the package name and to emphasize the fact that
they’re not parsed as proper operators.

Once the package *doubt* is attached a dubious operator can be built
from any sequence of characters containing at least one `?` as long as
`x <dubious op> y` is syntactic.

It is defined like any regular function, but using backticks, for
instance :

``` r
library(doubt)
#> 
#> Attaching package: 'doubt'
#> The following object is masked from 'package:utils':
#> 
#>     ?
`?!!` <- function(x) paste(x,"!!")
?!! "hello"
#> [1] "hello !!"
```

In practice we advise to stick to some clear templates to avoid
confusion.

To be recognized by *doubt* an operator must be either defined in the
global environment, be defined in a package AND registered (see
dedicated section), or have the standard `?fun?` format, where `fun` is
accessible from where `?fun?` is called.

Standard usage as documented in `?utils::Question` still works.

## Installation

Install with :

``` r
remotes::install_github("moodymudskipper/doubt")
```

# call standard functions with dubious syntax

Any function call `fun(x, y, z)` can be called by `?fun? x ? y ? z`.

``` r
print(search())
#>  [1] ".GlobalEnv"        "package:doubt"     "package:stats"    
#>  [4] "package:graphics"  "package:grDevices" "package:utils"    
#>  [7] "package:datasets"  "package:methods"   "Autoloads"        
#> [10] "package:base"
```

``` r
?sqrt? 16
#> [1] 4

?head? cars ? 2
#>   speed dist
#> 1     4    2
#> 2     4   10

cars ?head? 2
#>   speed dist
#> 1     4    2
#> 2     4   10

?paste? "a" ? "b" ? "c" ? "d"
#> [1] "a b c d"

"a" ?paste? "b" ? "c" ? "d"
#> [1] "a b c d"
```

Named arguments are not supported at the moment.

# binary syntax to define special assignment functions

The binary syntax of dubious operators is well suited to design special
assignment functions or to write to files or to connections, in the
examples below:

  - The first function modifies the lhs in place by applying the
    function in the rhs.

  - The second function writes a csv.

<!-- end list -->

``` r
`<-apply?` <- function(x, fun) {
  assign(deparse(substitute(x)), fun(x), envir = parent.frame())
  invisible()
}

x <- "abc"
x <-apply? toupper
x
#> [1] "ABC"

`<-csv?` <- function(file, data) {
  write.csv(data, file)
  invisible()
}
path <- tempfile(fileext = ".csv") 
path <-csv? head(cars, 3)
read.csv(path)
#>   X speed dist
#> 1 1     4    2
#> 2 2     4   10
#> 3 3     7    4
```

## Unary syntax to execute code in another context

The unary syntax is well suited to define functions that will execute
the rhs in another context, either another environment or even another
language.

We propose an example withe the *Julia* language, using the package
*JuliaCall*.

This should probably live in another package when *doubt* gets more
mature, but for now it serves to showcase the potential of *doubt*.

we defined an operator `?julia>` which allows us to run code in a
*Julia* session, `?julia>` doesn’t return any object to R (though it
print the output of the *Julia* console) but its sister function
`?!julia>` does.

``` r
JuliaCall::julia_setup("C:/Users/afabri/AppData/Local/Julia-1.2.0/bin")
#> Julia version 1.2.0 at location C:\Users\afabri\AppData\Local\JULIA-~1.0\bin will be used.
#> Loading setup script for JuliaCall...
#> Finish loading setup script for JuliaCall.
?julia> "a = 5"
#> starting httpd help server ...
#>  done
?!julia> "a * 2"
```

Julia’s syntax is not so different from R’s so most Julia code is
syntactic in R. We offer the possibility to skip the quotes in this
case. This way we leverage syntax highlighting and feel much more as if
we’re coding in *Julia* directly.

``` r
?julia> 'ceil(3.2)'
?julia> ceil(3.2)
```

For some issues of syntax compatibility some workaround were
implemented, for examples *Julia* assignments, if not quoted, should be
done using `:=`.

``` r
?julia> 'x = 42'
?julia> x := 42
```

Going to the next line should be done by typing `++`, and not `;`.

``` r
?julia> "z = begin
  x = 1
  y = 2
  x + y
  end"
#> 3

?julia> z := begin ++
  x := 1 ++
  y := 2 ++
  x + y ++
  end
#> 3
```

And terms separated by spaces in *Julia* can be separated by *?* in R to
keep the expression syntactic.

``` r
?julia> 'if 1 < 2 "hello" end'
#> "hello"
?julia> `if` ? 1 < 2 ? "hello" ? end
#> "hello"
```

## including dubious operators in packages

Packages using doubt should either depend on *doubt* or import it and
reexport `?`.

In the future *doubt* should export only `?` so it should be clean to
import it, so we don’t have different versions of `?` overriding each
other.

In any case these packages should define an `.onAttach` function which
will increment the `options("doubt.registered_ops")` with the names of
the dubious operators of the package.

It should look like this:

``` r
.onAttach <- function(libname, pkgname) {
  current_ops <- getOption("doubt.registered_ops")
  package_ops <- c("?julia>", "?!julia>")
  # functions are sorted in decreasing order so longer function is matched
  # first in case of ambiguity
  options(doubt.registered_ops = sort(union(current_ops, package_ops), TRUE))
  invisible()
}
```
