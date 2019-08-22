regex.escape <- function(string) {
  gsub("([][{}()+*^$|\\\\?])", "\\\\\\1", string)
}

double_qm_pattern <- "^.*?\\?(.*?)\\?.*$"

#' Modified question mark operator
#'
#' `?` was modified to allow definition of new operators (unary, binary or n-ary).
#'   We refer to those as "dubious" operators, both as a reference to the
#'   package name and to emphasize the fact that they're not parsed as proper
#'   operators.
#' .
#' Standard usage as documented in `?utils::Question` still works.
#'
#' Define any operator starting with `?` and that can be used in a way that is
#' syntactically valid and it will be executed with the same operator precedence
#' as `?`, which is just below assignment operations.
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
  txt <- rlang::expr_deparse(call)
  # repair expression if they started with "??"
  txt <- sub("\\? \\(\\?(.*)\\)$","??\\1",txt)
  # look in parent environment for operators starting with `?`
  ops <- ls(pattern = "\\?", envir = .GlobalEnv)
  #ops <- ls(pattern = "^\\?.+$", envir = .GlobalEnv)
  # add the registered operators to the list
  ops <- c(ops,getOption("doubt.registered_ops"))
  # Make patterns out of those, allowing for extra spaces
  patterns <- sapply(strsplit(ops,""), function(x) paste(regex.escape(x), collapse="\\s*"))
  # find all matches
  matches_lgl <- sapply(patterns, grepl, txt, USE.NAMES = FALSE)
  if(any(matches_lgl)){
    # take the last match, which should be the longest in case of multiple matches
    match_index <- length(ops) + 1 - which.max(rev(matches_lgl))
    # split the statement in two around the operator
    lhs_rhs <- strsplit(txt, patterns[match_index])[[1]]
    # remove empty side if relevant
    if(lhs_rhs[[1]] == "") lhs_rhs <- lhs_rhs[-1]
    # split further by question mark symbol to get the arguments
    args_chr <- trimws(unlist(strsplit(lhs_rhs,"?", TRUE)))
    # parse the arguments
    args <- unname(sapply(args_chr, parse, file="", n = NULL))
    # build call from relevant function and parsed arguments
    call <- as.call(c(as.symbol(ops[match_index]),args))
    # execute call in calling environment
    eval.parent(call)
  } else if (grepl(double_qm_pattern, txt)){
    fun_chr <- trimws(gsub(double_qm_pattern, "\\1", txt))
    op <- get0(fun_chr)
    if(is.null(op)) stop(sprintf("no function named '%s' was found", fun_chr))
    lhs_rhs <- strsplit(txt, paste0("\\?\\s*",fun_chr," \\?"))[[1]]
    args_chr <- trimws(unlist(strsplit(lhs_rhs,"?", TRUE)))
    args <- unname(sapply(args_chr, parse, file="", n = NULL))
    call <- as.call(c(as.symbol(fun_chr),args))
    eval.parent(call)
  } else {
    `?` <- utils::`?`
    eval(call)
  }
}
