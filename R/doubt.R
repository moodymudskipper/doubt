regex.escape <- function(string) {
  gsub("([][{}()+*^$|\\\\?])", "\\\\\\1", string)
}

#' Modified question mark operator
#'
#' `?` was modified to allow definition of new operators.
#' Standard usage as documented in `?utils::Question` still works.
#'
#' Define any operator starting with `?` and that can be used in a way that is
#' syntactically valid and it will be executed with the same operator precedence
#' as `?`, which is just below assignment operations.
#'
#' This precedence makes these operators well suited for modified assignments or
#' effects, where standard `%foo%` infix operators can sometimes are problematic.
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
#' # more useful : assign a data frame to a csv file
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
  ops <- ls(pattern = "^\\?.+$",envir = parent.frame())
  # Make patterns out of those, allowing for extra spaces
  patterns <- sapply(strsplit(ops,""), function(x) paste(regex.escape(x), collapse="\\s*"))
  # find all matches
  matches_lgl <- sapply(patterns, grepl, txt, USE.NAMES = FALSE)
  if(any(matches_lgl)){
    # take the last match, which should be the longest in case of multiple matches
    match_index <- length(ops) + 1 - which.max(rev(matches_lgl))
    # redefine `?` as new op
    `?` <- get(ops[match_index])
    # replace pattern in call
    txt <- gsub(patterns[match_index],"?",txt)
    # execute new call with new definition of `?`
    eval(parse(text=txt)[[1]])
  } else {
    `?` <- utils::`?`
    eval(call)
  }
}
