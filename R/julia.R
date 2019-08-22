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
    code <- paste(args, collapse = " ")
    code <- gsub(reserved_regex, "\\1", code)
    code <- gsub(":=", "=", code,fixed = TRUE)
    code <- gsub("+ +", "\n", code,fixed = TRUE)
  }
  fun(code)
}


#' @export
#' @rdname name julia
`?!julia>` <- function(...) `?julia>`(..., fun = JuliaCall::julia_eval)
