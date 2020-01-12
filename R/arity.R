# is_syntactic <- function(x){
#   tryCatch({str2lang(x); TRUE},
#            error = function(e) FALSE)
# }

is_syntactic_symbol <- function(x){
  make.names(x) == x
}


# #' @export
# arity <- function(x){
#   arity <- c(
#   if(is_syntactic(paste(x,"bar"))) "unary",
#   if(is_syntactic(paste("foo",x,"bar"))) "binary")
#   if(is.null(arity))
#     return("invalid")
#   if("unary" %in% arity && !identical(quote(`?`), str2lang(paste(x,"bar"))[[1]]))
#     return("invalid")
#   if("binary" %in% arity && !identical(quote(`?`), str2lang(paste("foo",x,"bar"))[[1]]))
#     return("invalid")
#
#   paste(arity,collapse="/")
# }

