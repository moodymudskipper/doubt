
deparse_rec <- function(call){
  if(!is.call(call)) return(deparse(call))
  call_deparsed <- lapply(call, deparse_rec)
  all_nms <- allNames(call_deparsed)
  all_nms <- all_nms[all_nms!=""]
  call_deparsed[all_nms] <-
    paste0(all_nms,"=",call_deparsed[all_nms])
  op <- call_deparsed[[1]]
  # for functions return the source
  if(op == "function") return(as.character(call[[4]]))
  # else choose a deparser
  if(length(call) > 2) {
    if(startsWith(op,"%")) op <- "%%"
    deparser <- switch(
      op,
      `while` = while_deparse, `for` = for_deparse,
      `if` = if_deparse,
      `?` = , `<-` = , `<<-` = , `=` = ,
      `:=` = , `~` = , `|` = , `||` = ,
      `&` = , `&&` = , `>` = , `>=` = ,
      `<` = , `<=` = , `==` = , `!=` = ,
      `+` = , `-` = , `*` = , `/` = ,
      # use unspaced_op_deparse instead of spaced_op_deparse for easier string play later
      `%%` = unspaced_op_deparse, `:` = ,
      `^` = , `$` = , `@` = , `::` = ,
      `:::` = unspaced_op_deparse,
      `[` = brackets_deparse, `[[` = brackets2_deparse,
      `{` = braces_deparse,
      std_fn_deparse)
  } else {
    deparser <- switch(
      op, `(` = parens_deparse, `{` = braces_deparse,
      `?` = , `~` = , `!` =  , `+`=, `-` = unary_op_deparse,
      `repeat` = repeat_deparse,
      std_fn_deparse)
  }
  do.call(deparser, call_deparsed)
}

while_deparse       <- function(...) sprintf("%s(%s) %s", ...)
for_deparse         <- function(...) sprintf("%s(%s in %s) %s", ...)
repeat_deparse      <- function(...) sprintf("%s %s", ...)
if_deparse          <- function(...) sprintf("%s(%s) %s else %s", ...)
#spaced_op_deparse   <- function(...) sprintf("%s %s %s", ..2, ..1, ..3)
unspaced_op_deparse <- function(...) sprintf("%s%s%s", ..2, ..1, ..3)
unary_op_deparse    <- function(...) sprintf("%s%s", ...)
brackets_deparse    <- function(...) sprintf("%s[%s]",..2,paste(c(...)[-1:-2], collapse = ", "))
brackets2_deparse   <- function(...) sprintf("%s[[%s]]",..2,paste(c(...)[-1:-2], collapse = ", "))
parens_deparse      <- function(...) sprintf("(%s)", ..2)
braces_deparse      <- function(...) sprintf("{%s}",paste(c(...)[-1], collapse = "\n"))
std_fn_deparse      <- function(...) sprintf("%s(%s)",..1,paste(c(...)[-1], collapse = ", "))
#unary_plus_fail     <- function(...) stop("Expressions containing unary `+` are not supported by dubious operators")


parse_qm_args <- function(x){
  x <- str2lang(x)
  # if single symbol
  if(length(x)==1) return(x)
  # if length 2 parse if it is a unary `?` call, else return as is
  if(length(x) == 2) {
    if (x[[1]] == quote(`?`)) return(x[[2]]) else return(x)
  }
  i <- numeric(0)
  out <- list()
  # as long as we have binary `?` calls
  while(length(x) == 3 && x[[1]] == quote(`?`)){
    out <- c(x[[3]],out)
    if(length(x[[3]]) == 2 && x[[c(3,1)]] == quote(`?`))
      stop("missing arguments between consecutive `?` are not supported")
    x <- x[[2]]
  }
  # if no `?` was found, return as is
  if(!length(out)) return(x)

  # if we have a unary `?` call remaining return its arg, else return remaining as is
  if(length(x) == 2 && x[[1]] == quote(`?`)) {
    x <- x[[2]]
    if(length(x) == 2 && x[[1]] == quote(`?`))
      stop("missing arguments between consecutive `?` are not supported")
    out <-  c(x,out)
  } else {
    out <-  c(x, out)
  }
  out
}
