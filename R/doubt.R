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
#' cars +head? 2
#' +head? cars
#' +head? {
#'   cars
#'   2}
#' @export
`?` <- function(e1,e2){
  call <- sys.call()
  # get called expression, or close enough
  # (`rlang::expr_deparse()` works better than `base::deparse()`)
  txt <- deparse_rec(call)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # handle `<op><fun>?` operators

  if (grepl(op_qm_pattern, txt)){
    call_and_ops <- build_op_qm_call_and_ops(txt, op_qm_pattern2)
    # message("call_and_ops$call")
    # print(call_and_ops$call)
    return(eval(call_and_ops$call, envir=call_and_ops$ops, enclos = parent.frame()))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # handle special syntax operators
  ops <- ls(pattern = "\\?", envir = .GlobalEnv)
  ops <- unique(c(ops,getOption("doubt.registered_syntaxes")))
  ops_compact <- gsub(" ", "", ops)
  ops_lgl <- sapply(ops_compact, unglue::unglue_detect, x = txt)
  if(any(ops_lgl)){
  if(sum(ops_lgl) > 1) {
    stop("Ambiguous syntax caused by operators: ", toString(ops[ops_lgl]))
  }

  op <- ops_compact[ops_lgl]
  args <- unglue::unglue(txt, op)[[1]]
  val <- eval.parent(str2lang(paste0("`",ops[ops_lgl], "`")))
  call_chr <- unglue::unglue_sub(val, val, args)
  return(eval.parent(str2lang(call_chr)))
  }

  if("devtools_shims" %in% search())
    `?` <- get("?", envir = as.environment("devtools_shims")) # nocov
  else
    `?` <- utils::`?`
  eval(call)
}
