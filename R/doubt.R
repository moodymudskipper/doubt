regex.escape <- function(string) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", string)
}

# two question marks surrounding any legal function name
double_qm_pattern <- "^[^?]*?(\\?([a-zA-Z.][a-zA-Z0-9._]*)\\?).*$"


op_qm_pattern <- "^.*?(([<]*[-+:*\\/<>!&|~=%^][%&|=]?)([a-zA-Z.][a-zA-Z0-9._]*)\\?).*$"
# the following to get several matches
op_qm_pattern2 <- "(([<]*[-+:*\\/<>!&|~=%^][%&|=]?)([a-zA-Z.][a-zA-Z0-9._]*)\\?)"

#dubious_pattern <- "^.*?[([0-9a-z_.]*)\\?.*$"

#' Modified question mark operator
#'
#' `?` was modified to allow definition of new operators (unary, binary or n-ary).
#'   We refer to those as "dubious" operators, both as a reference to the
#'   package name and to emphasize the fact that they're not parsed as proper
#'   operators.
#' .
#' Standard usage as documented in `?utils::Question` still works.
#'
#' Every accessible function, custom defined or base/packaged, can be called as
#'  an infix operator, for example `1:5 %%intersect? 3:7` is equivalent to
#'  `intersect(1:5, 3:7)`. In that case, `%%intersect?` will have the precedence
#'  of `%%`, which is the most intuitive,
#' but any precedence including and below unary `+` can be used, for instance
#' `*intersect?` will have the precedence of `*`.
#'
#' N-ary operators are supported for `?foo?` operators, for instance
#' `?paste? "a" ? "b" ? "c"` is the same as `paste("a", "b", "c")`,
#' `"a" ?paste? "b" ? "c"` works as well.
#'
#' Define any operator containing `?` and that can be used in a way that is
#' syntactically valid and it will be executed with the same operator precedence
#' as `?`, which is just below `<-` and just above `=` (note that `?Syntax` is
#' inaccurate in this regard at time of writing). In practice this mean you can
#' assign with `x = "a" ?paste? "b" ? "c"` but not with `x <- "a" ?paste? "b" ? "c"`
#'
#'
#' @param e1 lhs
#' @param e2 rhs
#' @examples
#' # multiplication with precedence of `?`
#' `?~` <- `*`
#' 1 + 2 ?~ 3 + 4
#' # division  with precedence of `?`
#' `?divide?` <- `/`
#' 1 + 2 ?divide? 3 + 4
#' # more useful : write a data frame to a csv file
#' \dontrun{
#' `?csv<-` <- function(e1, e2) eval.parent(substitute(write.csv(e2, e1)))
#' tempfile() ?csv<- cars
#' }
#' @export
`?` <- function(e1,e2){
  call <- sys.call()
  # get called expression, or close enough
  # (`rlang::expr_deparse()` works better than `base::deparse()`)
  txt <- deparse_rec(call)
  # look in parent environment for operators starting with `?`
  ops <- ls(pattern = "\\?", envir = .GlobalEnv)
  # add the registered operators to the list
  ops <- c(ops,getOption("doubt.registered_ops"))
  # Make patterns out of those, allowing for extra spaces
  patterns <- regex.escape(ops)
  # find all matches
  matches_lgl <- sapply(patterns, grepl, txt, USE.NAMES = FALSE)

  if(any(matches_lgl)){
    # if we match a registered/packaged/globally defined op
    call <- build_registered_op_call(txt, matches_lgl, patterns, ops)
    # execute call in calling environment
    return(eval.parent(call))
  } else if (grepl(double_qm_pattern, txt)){
    # handle `?<fun>?` operator if what we found is relevant
    call <- build_double_qm_call(txt, double_qm_pattern)
    if(!is.null(call)) return(eval.parent(call))
  }

  if (grepl(op_qm_pattern, txt)){
    # handle `<op><fun>?` operator if what we found is relevant
    call_and_ops <- build_op_qm_call_and_ops(txt, op_qm_pattern2)
    return(eval(call_and_ops$call, envir=call_and_ops$ops, enclos = parent.frame()))

    # call_and_1op <- build_op_qm_call_and_1op(txt, op_qm_pattern)
    # return(eval(call_and_1op$call, envir=call_and_1op$op, enclos = parent.frame()))
  }

  `?` <- get_fallback_qm()
  eval(call)
}





#
# regularize <- function(call){
#   if(!is.call(call)) return(call)
#   if(calls_unary_plus(call)){
#     call[[1]] <- op
#     call[[2]] <- call[[2]][[1]]
#     call
#   } else {
#     as.call(lapply(call, regularize))
#   }
# }
#
#
# calls_unary_plus <- function(call){
#   length(call) > 1 && length(call[[3]]) == 2 && identical(call[[3]][[1]],quote(`+`))
# }
#
# call <- quote(y -+a / b)
# calls_unary_plus(quote(y /+a + b))
