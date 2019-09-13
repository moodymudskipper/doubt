
# iris2 <- iris[c(1:2,51:52),]
# iris2 %>% select(?is.numeric)
# foo <- c("Sepal.Length", "Species")
# iris2 %>% select(?)
# iris2 %>% gather(.,,,?is.numeric)
# iris2 %>% group_by(Species) %>% summarize(!!!?is.numeric := mean)
# iris2 %>% group_by(Species) %>% summarize(nested = ?is.numeric := mean)

easter_summarize_splice <- function(expr){
  expr <- substitute(expr)
  . <- eval.parent(quote(.),4)
  group_vars <- dplyr::group_vars(.)


  if (is.symbol(expr)) expr <- list(expr)
  if(expr[[1]] != quote(`:=`)){
    lhs <- eval(expr[[1]])
    apply_fun <- identity
  } else {
    lhs <- eval(expr[[2]])
    apply_fun <- rlang::as_function(eval(expr[[3]]))
  }

  if(is.function(lhs) || inherits(lhs, "formula")){
    detect_fun <- rlang::as_function(lhs)
    vars <- setdiff(names(.),group_vars)
    vars <- vars[sapply(.[vars], detect_fun)]
  } else {
    vars <- lhs
  }
  res <- summarize_at(.,vars, apply_fun) %>%
    ungroup() %>%
    select(-one_of(group_vars))
  res
}

easter_summarize_nest <- function(expr){
  stop("calls to `?` in `summarize` must be spliced, use `!!!?`")
}

easter_mutate_splice <- function(expr){
  expr <- substitute(expr)
  . <- eval.parent(quote(.),4)
  group_vars <- dplyr::group_vars(.)


  if (is.symbol(expr)) expr <- list(expr)
  if(expr[[1]] != quote(`:=`)){
    lhs <- eval(expr[[1]])
    apply_fun <- identity
  } else {
    lhs <- eval(expr[[2]])
    apply_fun <- rlang::as_function(eval(expr[[3]]))
  }

  if(is.function(lhs) || inherits(lhs, "formula")){
    detect_fun <- rlang::as_function(lhs)
    vars <- setdiff(names(.),group_vars)
    vars <- vars[sapply(.[vars], detect_fun)]
  } else {
    vars <- lhs
  }
  #if(expr[[1]] != quote(`:=`)) stop("invalid expression, form must be `?detect_fun := apply_fun`")

  res <- transmute_at(.,vars, apply_fun) %>%
    ungroup() %>%
    select(-one_of(group_vars))
  res
}

easter_mutate_nest <- function(expr){
  stop("calls to `?` in `summarize` must be spliced, use `!!!?`")
}

easter_select_gather <- function(fun) {
  vars <- eval.parent(quote(tidyselect::peek_vars()))
  . <- eval.parent(quote(.),4)
  if(is.function(fun)){
    vars_lgl <- sapply(vars, function(x) fun(.[[x]]))
  } else if (is.character(fun)){
    vars_lgl <- fun %in% names(.)
  }
  which(vars_lgl)
}

get_fallback_qm <- function(){
  if(length(sys.calls()) >= 6 &&
     identical(sys.call(-5), quote(vars_select_eval(.vars, quos)))){
    `?` <- easter_select_gather
  } else if(length(sys.calls()) >= 10 && identical(sys.call(-9)[[1]], quote(summarize))){
    `?` <- easter_summarize_splice
  } else if(length(sys.calls()) >= 3 && identical(sys.call(-2)[[1]], quote(summarise_impl))){
    `?` <- easter_summarize_nest
  } else if(length(sys.calls()) >= 10 && identical(sys.call(-9)[[1]], quote(mutate))){
    `?` <- easter_mutate_splice
  } else if(length(sys.calls()) >= 3 && identical(sys.call(-2)[[1]], quote(mutate_impl))){
    `?` <- easter_mutate_nest
  } else if(length(sys.calls()) >= 10 && identical(sys.call(-9)[[1]], quote(transmute))){
    `?` <- easter_mutate_splice
  } else if(length(sys.calls()) >= 3 && identical(sys.call(-2)[[1]], quote(transmute_impl))){
    `?` <- easter_mutate_nest
  } else {
    `?` <- utils::`?`
  }
}
