#' Register Dubious Syntaxes
#'
#' To use a dubious syntax in a package, use this function in the definition of `.onAttach`
#' @param syntaxes a character vector of the syntaxes to support
#'
#' @export
#' @examples
#' \dontrun{
#' # define your syntax as you would define a normal function
#' `?add> {x} : {y}` <- function(x, y) x + y
#'
#' # register the syntax in your .onAttach definition
#' .onAttach <- function(libname, pkgname) {
#' doubt::register_dubious_syntaxes("?add> {x} : {y}")
#' invisible()
#' }
#' }
register_dubious_syntaxes <- function(syntaxes){
  registered_syntaxes <- unlist(getOption("doubt.registered_syntaxes"))
  # combine and sort starting with longest to avoid ambiguity
  all_syntaxes <- union(registered_syntaxes, syntaxes)
  all_syntaxes <- all_syntaxes[order(nchar(all_syntaxes),decreasing = TRUE)]
  options(doubt.registered_syntaxes = all_syntaxes)
}
