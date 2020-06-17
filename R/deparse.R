ops <- c("function", "repeat","while", "for","if",
         "?", "<-", "<<-", "=",
         ":=", "~", "|", "||",
         "&", "&&", ">", ">=",
         "<", "<=", "==", "!=",
         "+", "-", "*", "/",
         "%%", ":",
         "^", "$", "@", "::",
         ":::",
         "[" , "[[" , "{", "(", "!")

deparse_rec <- function(call, first = FALSE){
  if(!is.call(call)) {
    if (is.symbol(call)){
      call <- as.character(call)
      if((first && call %in% ops) || is_syntactic_symbol(call))
        return(call)
      return(paste0("`",call,"`"))
    } else
      return(deparse(call))
  }
  call_deparsed <- c(list(deparse_rec(call[[1]], first = TRUE)),lapply(call[-1], deparse_rec))
  all_nms <- allNames(call_deparsed)
  all_nms <- all_nms[all_nms!=""]
  call_deparsed[all_nms] <-
    paste0(all_nms,"=",call_deparsed[all_nms])
  op <- call_deparsed[[1]]
  # for functions return the source
  if(op == "function") {
    return(paste0(sub("\\{.*$","",as.character(call[[4]])[[1]]), call_deparsed[[3]]))
  }
  # else choose a deparser
  if(length(call) > 2) {
    if(startsWith(op,"%")) op <- "%%"
    deparser <- switch(
      op,
      #`repeat` = repeat_deparse,
      `while` = while_deparse, `for` = for_deparse,
      `if` = if_deparse,
      `<` = if(startsWith(call_deparsed[[3]],"-")) broken_arrow_deparse else
        unspaced_op_deparse,
      `?` = , `<-` = , `<<-` = , `=` = ,
      `:=` = , `~` = , `|` = , `||` = ,
      `&` = , `&&` = , `>` = , `>=` = ,
      `<=` = , `==` = , `!=` = ,
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
unspaced_op_deparse <- function(...) sprintf("%s%s%s", ..2, ..1, ..3)
broken_arrow_deparse <- function(...) sprintf("%s%s %s", ..2, ..1, ..3)
unary_op_deparse    <- function(...) sprintf("%s%s", ...)
brackets_deparse    <- function(...) sprintf("%s[%s]",..2,paste(c(...)[-1:-2], collapse = ", "))
brackets2_deparse   <- function(...) sprintf("%s[[%s]]",..2,paste(c(...)[-1:-2], collapse = ", "))
parens_deparse      <- function(...) sprintf("(%s)", ..2)
braces_deparse      <- function(...) sprintf("{%s}",{
  paste(c(...)[-1], collapse = ";")})
std_fn_deparse      <- function(...) sprintf("%s(%s)",..1,paste(c(...)[-1], collapse = ", "))
