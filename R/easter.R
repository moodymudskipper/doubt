# as_function <- function(...){
#   if(!requireNamespace("rlang")) stop("you'll need the package 'rlang' to use this feature")
#   f <- utils::getFromNamespace("as_function", "rlang")
#   f(...)
# }
# group_vars <- function(...){
#   if(!requireNamespace("dplyr")) stop("you'll need the package 'dplyr' to use this feature")
#   f <- utils::getFromNamespace("group_vars", "dplyr")
#   f(...)
# }
# vars <- function(...){
#   if(!requireNamespace("dplyr")) stop("you'll need the package 'dplyr' to use this feature")
#   f <- utils::getFromNamespace("vars", "dplyr")
#   f(...)
# }
# peek_vars <- function(...){
#   if(!requireNamespace("tidyselect")) stop("you'll need the package 'tidyselect' to use this feature")
#   f <- utils::getFromNamespace("peek_vars", "tidyselect")
#   f(...)
# }
# `%>%` <- function(...){
#   if(!requireNamespace("magrittr")) stop("you'll need the package 'magrittr' to use this feature")
#   f <- utils::getFromNamespace("%>%", "magrittr")
#   f(...)
# }
#
# existsFunction <- utils::getFromNamespace("existsFunction", "methods")
# allNames <- utils::getFromNamespace("allNames", "methods")
#
# qm_summarize_splice <- function(expr){
#   expr <- substitute(expr)
#   data <- eval.parent(quote(.),4)
#   group_vars <- group_vars(data)
#
#   # maybe no `:=` should mean all ?
#
#   # if (is.symbol(expr)) expr <- list(expr)
#
#   if(is.symbol(expr) || expr[[1]] != quote(`:=`)){
#     lhs <- eval(quote(vars(everything())))
#     apply_fun <- as_function(eval(expr))
#   } else {
#     lhs <- eval(expr[[2]])
#     apply_fun <- as_function(eval(expr[[3]]))
#   }
#
#   if(is.function(lhs) || inherits(lhs, "formula")){
#     detect_fun <- as_function(lhs)
#     vars <- setdiff(names(data),group_vars)
#     vars <- vars[sapply(data[vars], detect_fun)]
#   } else {
#     vars <- lhs
#   }
#   res <- summarize_at(data,vars, apply_fun) %>%
#     ungroup() %>%
#     select(-one_of(group_vars))
#   res
# }
#
# qm_mutate_splice <- function(expr){
#   expr <- substitute(expr)
#   data <- eval.parent(quote(.),4)
#   group_vars <- group_vars(data)
#
#   if(
#     # if is function
#     is.symbol(expr) && !is.null(get0(deparse(expr), mode = "function")) ||
#     # or is litteral formula
#     is.call(expr) && expr[[1]] == quote(`~`)
#     ){
#     # apply to all
#     lhs <- eval(quote(vars(everything())))
#     apply_fun <- as_function(eval(expr))
#   } else {
#     if(expr[[1]] != quote(`:=`)){
#       lhs <- eval(expr)
#       apply_fun <- identity
#     } else {
#       lhs <- eval(expr[[2]])
#       apply_fun <- as_function(eval(expr[[3]]))
#     }
#   }
#
#   if(is.function(lhs) || inherits(lhs, "formula")){
#     detect_fun <- as_function(lhs)
#     vars <- setdiff(names(data),group_vars)
#     vars <- vars[sapply(data[vars], detect_fun)]
#   } else {
#     vars <- lhs
#   }
#   # remove group vars
#   if(length(group_vars)){
#     if (is.character(vars)) vars <- vars(!!!vars)
#     vars <- c(vars, vars(!!!as.list(parse(text=paste0("-",group_vars)))))
#     }
#   res <- transmute_at(data,vars, apply_fun) %>%
#     ungroup() %>%
#     select(-one_of(group_vars))
#   res
# }
#
# qm_select_gather <- function(fun) {
#   vars <- eval.parent(quote(peek_vars()))
#   data <- eval.parent(quote(.),4)
#   vars_lgl <- sapply(vars, function(x) fun(data[[x]]))
#   which(vars_lgl)
# }
#
# get_fallback_qm <- function(){
#   all_calls <- as.character(lapply(sys.calls(), `[[`, 1))
#   funs <- c("summarize","summarize", "mutate", "transmute", "select", "gather")
#   funs_lgl <- funs %in% rev(all_calls)
#   if(!any(funs_lgl)) return(utils::`?`)
#
#   # find in call stack the last of those functions that was called
#   fun <- funs[which.max(funs_lgl)]
#   switch(fun,
#          summarize=,
#          summarise = qm_summarize_splice,
#          transmute=,
#          mutate = qm_mutate_splice,
#          gather=,
#          select= qm_select_gather
#   )
# }
