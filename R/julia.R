



reserved <-
  c("if", "else", "repeat", "while", "function", "for", "in", "next", "break",
    "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
    "NA_complex_", "NA_character_")

reserved_regex <- paste0("`(", paste0("(",reserved,")", collapse="|"),")`")

#' Run julia code
#'
#' @export
#' @name julia
`?julia>` <- function(..., fun = JuliaCall::julia_command){
  if(!requireNamespace("JuliaCall"))
    stop("Install package JuliaCall to use `?julia>` function")
  args <- eval(substitute(alist(...)))
  if(length(args) == 1 && is.character(args[[1]])){
    code <- args[[1]]
  } else {
    args <- lapply(args, rlang::expr_deparse)
    # `"?"` symbols split the argument in doubt::`?`, are we paste them back
    # so it acts as if `" ? "` in the code is replaced by spaces
    code <- paste(args, collapse = " ")
    # reserved words such as `if` etc, and only them, can be used with backticks
    # backticks are used in Julia to execute commands but these few exceptions
    # are expected no to conflict
    code <- gsub(reserved_regex, "\\1", code)
    # the parser doesn't like `? x <- y`, so we need to use `? x := y`
    code <- gsub(":=", "=", code,fixed = TRUE)
    # to go to the next line we will use `++` (deparsed as `"+ +"`)
    code <- gsub("+ +", "\n", code,fixed = TRUE)
    # # we can use a dot just next to a bracket to make it syntactic, it is not
    # # ambiguous as Julia variable names can't be a dot nor start or end with dots
    # code <- gsub("\\.([[({])", "\\1", code,fixed = TRUE)
  }
  fun(code)
}

#' @export
#' @rdname name julia
`?!julia>` <- function(...) `?julia>`(..., fun = JuliaCall::julia_eval)

