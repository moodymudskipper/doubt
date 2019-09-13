
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doubt

*doubt* modifies the operator `?` to allow definition of new operators
(unary, binary or n-ary). We refer to those as “dubious” operators, both
as a reference to the package name and to emphasize the fact that
they’re not parsed as proper operators.

### Installation

Install with :

``` r
remotes::install_github("moodymudskipper/doubt")
```

### Standard usage

Standard usage as documented in `?utils::Question` still works.

### Call any accessible function as an infix operator

Once the package *doubt* is attached, any accessible function can be
called as an infix operator. See example below:

``` r
library(doubt, warn.conflicts = FALSE)
1:4 %%intersect? 3:8
#> [1] 3 4
```

The precedence is given by the first symbol of the operator, so we can
also do things like:

``` r
cars *head? 2 +rbind? cars *tail? 2
#>    speed dist
#> 1      4    2
#> 2      4   10
#> 49    24  120
#> 50    25   85
```

For most cases sticking with `%%fun?` dubious operators will be more
intuitive and readable as we’re used to `%fun%` operators and their
precedence.

User defined functions will work just the same :

``` r
AND <- function(x,y) paste(x,"AND",y)
"this" %%AND? "that" 
#> [1] "this AND that"
```

`<-fun?` operators works consistently with the principles outlined
above.

``` r
# in place append
append2 <- function(x, values){
  parent <- parent.env(environment())
  assign(deparse(substitute(x)), append(x, values),envir = parent)
}
x <- 1:2
x <-append2? 11:12 
x
#> [1]  1  2 11 12
```

It would be convenient however to have the assignment behavior by
default and be able to do just `x <-append? 11:12` in the case above.
This might be implemented in near future and will break the above
behavior.

The `?fun?` operators are a special case outlined in the next section.

### `?fun?` n-ary operators

`?fun?` operators are n-ary and have the precedence of `?`, which is
right between `=` (which has the lowest precedence, despite what is
documented in `?Syntax`), and `<-`.

Any function call `fun(x, y, z)` can be called by `?fun? x ? y ? z`, as
in the following example :

``` r
# in binary form, works as other dubious ops
cars ?subset? speed == 25
#>    speed dist
#> 50    25   85
# equivalent to : 
?subset? cars ? speed == 25
#>    speed dist
#> 50    25   85
# We can add more arguments
cars ?subset? speed == 25 ? "dist"
#>    dist
#> 50   85
# equivalent to
?subset? cars ? speed == 25 ? "dist"
#>    dist
#> 50   85
```

Note that because of operator precedence `res <- cars ?subset? speed
== 25` will not assign `cars ?subset? speed == 25` to `res`. In that
case we can do either of the following :

``` r
# Use `=` as it has the lowest precedence
res1 = cars ?subset? speed == 25
res1
#>    speed dist
#> 50    25   85
res2 = ?subset? cars ? speed == 25
res2
#>    speed dist
#> 50    25   85
```

Named arguments are not supported at the moment.

Note that because of the very low precedence of these operators and the
ambiguity that wold come from the use of the n-ary notation, only one
`?fun?` operator can be used by statement.

### *piping* with any precedence

We sometimes run into precedence issues when using *magrittr*’s piping
operator `%>%`. *doubt* wraps this operator and it can be called with
the desired precedence, which is convenient when we want to pipe to a
sum for example.

A common use case is the usage of `plotly::ggplotly()` on the result of
a ggplot call. The following will produce an interractive *plotly*
chart.

``` r
library(ggplot2)
library(plotly)
ggplot(cars,aes(speed, dist)) + geom_point() +.? ggplotly()
# equivalent to :
ggplotly(ggplot(cars,aes(speed, dist)) + geom_point())
```

### Custom operators

It’s generally wiser to define a regular function and rely on the
behaviors described in above sections but one might want to define a
`?!` operator for instance.

Doing so is as simple defining it like any regular function, for
instance :

``` r
`?!` <- function(x) paste(x,"!!")
?! "hello"
#> [1] "hello !!"
```

It will have `?`’s precedence and will allow n-ary notation just like
`?fun?` operators :

``` r
`?!?` <- function(x,y) paste(x,y,"!!")
?!? "hello" ? "world"
#> [1] "hello world !!"
```

Dubious operators can be built from any sequence of characters
containing at least one `?` as long as `x <dubious op> y` is syntactic
and that the first `?` is binary (it must be able to “see” the lhs).

The latter statement means that `?!` and `!.?` are allowed but `!?` is
not. The function `arity` can be used to clear confusion, it returns
`"invalid"` for invalid dubious ops, and the arity of the operator
otherwise (which is decided by the first symbol of the operator).

``` r
arity("?!")
#> [1] "unary/binary"
arity("?!?")
#> [1] "unary/binary"
arity("!?")
#> [1] "invalid"
arity("!.?")
#> [1] "unary"
```

To be recognized by *doubt* a custom operator must either :

  - be defined in the global environment
  - be defined in a package AND registered

### Registering a custom dubious op in a package

To use a custom dubious operator your package should either depend on
*doubt* or import it and reexport `?`.

In the future *doubt* should export only `?` so it should be clean to
import it, so we don’t have different versions of `?` overriding each
other.

Additionally you should add an `.onAttach` function to your package
(usually done in a `zz.R` file) or edit yours so it updates *doubt*’s
options, as in the following :

``` r
.onAttach <- function(libname, pkgname) {
  doubt::register_ops(c("?!", "?!!"))
  invisible()
}
```

### *dplyr* extension

You can use a convenient syntax to combine the functionality of *dplyr*
verbs with their suffixed `*_if` / `*_at` variants.

This won’t be documented once the package reaches maturity, because :

  - the implementation is very hacky
  - it doesn’t reall fit into the scope of the package
  - it works only after a pipe, which can be confusing

However I might leave the feature as an Easter egg as I like to use it
interactively.

``` r
library(dplyr)
#> Warning: package 'dplyr' was built under R version 3.6.1
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
starwars2 <- starwars[1:2,c(1:3,10:12)]
starwars2
#> # A tibble: 2 x 6
#>   name           height  mass species films     vehicles 
#>   <chr>           <int> <dbl> <chr>   <list>    <list>   
#> 1 Luke Skywalker    172    77 Human   <chr [5]> <chr [2]>
#> 2 C-3PO             167    75 Droid   <chr [6]> <chr [0]>
starwars2 %>% select(name, ?is.numeric)
#> # A tibble: 2 x 3
#>   name           height  mass
#>   <chr>           <int> <dbl>
#> 1 Luke Skywalker    172    77
#> 2 C-3PO             167    75
starwars2 %>% mutate(!!!?is.numeric := ~./100, !!!?is.character := toupper)
#> # A tibble: 2 x 6
#>   name           height  mass species films     vehicles 
#>   <chr>           <dbl> <dbl> <chr>   <list>    <list>   
#> 1 LUKE SKYWALKER   1.72  0.77 HUMAN   <chr [5]> <chr [2]>
#> 2 C-3PO            1.67  0.75 DROID   <chr [6]> <chr [0]>
starwars2 %>% transmute(!!!?is.numeric := ~./100, !!!?is.character)
#> # A tibble: 2 x 4
#>   height  mass name           species
#>    <dbl> <dbl> <chr>          <chr>  
#> 1   1.72  0.77 Luke Skywalker Human  
#> 2   1.67  0.75 C-3PO          Droid

starwars %>% 
  group_by(gender) %>% 
  summarize(
    # `_if` case
    !!!?is.numeric := mean, 
    # standard case
    name = toString(name),
    # `_at` case
    !!!?vars(starts_with("spec")) := ~toString(unique(.)))
#> # A tibble: 5 x 6
#>   gender   height   mass birth_year name               species             
#>   <chr>     <dbl>  <dbl>      <dbl> <chr>              <chr>               
#> 1 female       NA   NA           NA Leia Organa, Beru~ Human, Twi'lek, Tho~
#> 2 hermaph~    175 1358          600 Jabba Desilijic T~ Hutt                
#> 3 male         NA   NA           NA Luke Skywalker, D~ Human, Wookiee, Rod~
#> 4 none         NA   NA           NA IG-88, BB8         Droid               
#> 5 <NA>        120   46.3         NA C-3PO, R2-D2, R5-~ Droid
```

<!-- # binary syntax to define special assignment functions -->

<!-- The binary syntax of dubious operators is well suited to design special assignment functions or to write to files or to connections, in the examples below: -->

<!-- * The first function modifies the lhs in place by applying the function in the rhs. -->

<!-- * The second function writes a csv. -->

<!-- ```{r, eval = FALSE} -->

<!-- `<-apply?` <- function(x, fun) { -->

<!--   assign(deparse(substitute(x)), fun(x), envir = parent.frame()) -->

<!--   invisible() -->

<!-- } -->

<!-- x <- "abc" -->

<!-- x <-apply? toupper -->

<!-- x -->

<!-- #> [1] "ABC" -->

<!-- `<-csv?` <- function(file, data) { -->

<!--   write.csv(data, file) -->

<!--   invisible() -->

<!-- } -->

<!-- path <- tempfile(fileext = ".csv")  -->

<!-- path <-csv? head(cars, 3) -->

<!-- read.csv(path) -->

<!-- #>   X speed dist -->

<!-- #> 1 1     4    2 -->

<!-- #> 2 2     4   10 -->

<!-- #> 3 3     7    4 -->

<!-- ``` -->

<!-- ### Unary syntax to execute code in another context -->

<!-- The unary syntax is well suited to define functions that will execute the rhs -->

<!-- in another context, either another environment or even another language. -->

<!-- We propose an example with the *Julia* language, using the package *JuliaCall*. -->

<!-- This should probably live in another package when *doubt* gets more mature, but -->

<!-- for now it serves to showcase the potential of *doubt*. -->

<!-- we defined an operator `?julia>` which allows us to run code in a *Julia* session, -->

<!-- `?julia>` doesn't return any object to R (though it print the output of the -->

<!-- *Julia* console) but its sister function `?!julia>` does. -->

<!-- ```{r} -->

<!-- JuliaCall::julia_setup("C:/Users/afabri/AppData/Local/Julia-1.2.0/bin") -->

<!-- ?julia> "a = 5" -->

<!-- ?!julia> "a * 2" -->

<!-- ``` -->

<!-- Julia's syntax is not so different from R's so most Julia code is syntactic in -->

<!-- R. We offer the possibility to skip the quotes in this case. This way we leverage -->

<!-- syntax highlighting and feel much more as if we're coding in *Julia* directly. -->

<!-- ```{r} -->

<!-- ?julia> 'ceil(3.2)' -->

<!-- ?julia> ceil(3.2) -->

<!-- ``` -->

<!-- For some issues of syntax compatibility some workaround were implemented, -->

<!-- for examples *Julia* assignments, if not quoted, should be done using `:=`. -->

<!-- ```{r} -->

<!-- ?julia> 'x = 42' -->

<!-- ?julia> x := 42 -->

<!-- ``` -->

<!-- Going to the next line should be done by typing `++`, and not `;`. -->

<!-- ```{r, eval = FALSE} -->

<!-- ?julia> "z = begin -->

<!--   x = 1 -->

<!--   y = 2 -->

<!--   x + y -->

<!--   end" -->

<!-- #> 3 -->

<!-- ?julia> z := begin ++ -->

<!--   x := 1 ++ -->

<!--   y := 2 ++ -->

<!--   x + y ++ -->

<!--   end -->

<!-- #> 3 -->

<!-- ``` -->

<!-- And terms separated by spaces in *Julia* can be separated by *?* in R to -->

<!-- keep the expression syntactic. -->

<!-- ```{r, eval = FALSE} -->

<!-- ?julia> 'if 1 < 2 "hello" end' -->

<!-- #> "hello" -->

<!-- ?julia> `if` ? 1 < 2 ? "hello" ? end -->

<!-- #> "hello" -->

<!-- ``` -->
