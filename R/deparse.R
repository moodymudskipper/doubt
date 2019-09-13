
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
      `repeat` = repeat_deparse, `if` = if_deparse,
      `?` = , `<-` = , `<<-` = , `=` = ,
      `:=` = , `~` = , `|` = , `||` = ,
      `&` = , `&&` = , `>` = , `>=` = ,
      `<` = , `<=` = , `==` = , `!=` = ,
      `+` = , `-` = , `*` = , `/` = ,
      `%%` = spaced_op_deparse, `:` = ,
      `^` = , `$` = , `@` = , `::` = ,
      `:::` = unspaced_op_deparse,
      `[` = brackets_deparse, `[[` = brackets2_deparse,
      `{` = braces_deparse,
      std_fn_deparse)
  } else {
    deparser <- switch(
      op, `(` = parens_deparse, `{` = braces_deparse,
      `?` = , `~` = , `!` =  , `+`=, `-` = unary_op_deparse,
      std_fn_deparse)
  }
  do.call(deparser, call_deparsed)
}

while_deparse       <- function(...) sprintf("%s(%s) %s", ...)
for_deparse         <- function(...) sprintf("%s(%s in %s) %s", ...)
repeat_deparse      <- function(...) sprintf("%s %s", ...)
if_deparse          <- function(...) sprintf("%s(%s) %s else %s", ...)
spaced_op_deparse   <- function(...) sprintf("%s%s%s", ..2, ..1, ..3)
unspaced_op_deparse <- function(...) sprintf("%s%s%s", ..2, ..1, ..3)
unary_op_deparse    <- function(...) sprintf("%s%s", ...)
brackets_deparse    <- function(...) sprintf("%s[%s]",..2,paste(c(...)[-1:-2], collapse = ", "))
brackets2_deparse   <- function(...) sprintf("%s[[%s]]",..2,paste(c(...)[-1:-2], collapse = ", "))
parens_deparse      <- function(...) sprintf("(%s)", ..2)
braces_deparse      <- function(...) sprintf("{%s}",paste(c(...)[-1], collapse = "\n"))
std_fn_deparse      <- function(...) sprintf("%s(%s)",..1,paste(c(...)[-1], collapse = ", "))
unary_plus_fail     <- function(...) stop("Expressions containing unary `+` are not supported by dubious operators")
