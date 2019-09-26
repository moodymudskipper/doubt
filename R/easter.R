
# iris2 <- iris[c(1:2,51:52),]
# iris2 %>% select(?is.numeric)
# foo <- c("Sepal.Length", "Species")
# iris2 %>% select(?)
# iris2 %>% gather(.,,,?is.numeric)
# iris2 %>% group_by(Species) %>% summarize(!!!?is.numeric := mean)
# iris2 %>% group_by(Species) %>% summarize(nested = ?is.numeric := mean)



qm_summarize_splice <- function(expr){
  expr <- substitute(expr)
  data <- eval.parent(quote(.),4)
  group_vars <- dplyr::group_vars(data)


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
    vars <- setdiff(names(data),group_vars)
    vars <- vars[sapply(data[vars], detect_fun)]
  } else {
    vars <- lhs
  }
  res <- summarize_at(data,vars, apply_fun) %>%
    ungroup() %>%
    select(-one_of(group_vars))
  res
}

qm_summarize_nest <- function(expr){
  stop("calls to `?` in `summarize` must be spliced, use `!!!?`")
}

qm_mutate_splice <- function(expr){
  expr <- substitute(expr)
  data <- eval.parent(quote(.),4)
  group_vars <- dplyr::group_vars(data)


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
    vars <- setdiff(names(data),group_vars)
    vars <- vars[sapply(data[vars], detect_fun)]
  } else {
    vars <- lhs
  }
  #if(expr[[1]] != quote(`:=`)) stop("invalid expression, form must be `?detect_fun := apply_fun`")

  res <- transmute_at(data,vars, apply_fun) %>%
    ungroup() %>%
    select(-one_of(group_vars))
  res
}

qm_mutate_nest <- function(expr){
  stop("calls to `?` in `summarize` must be spliced, use `!!!?`")
}

qm_select_gather <- function(fun) {
  vars <- eval.parent(quote(tidyselect::peek_vars()))
  data <- eval.parent(quote(.),4)
  if(is.function(fun)){
    vars_lgl <- sapply(vars, function(x) fun(data[[x]]))
  } else if (is.character(fun)){
    vars_lgl <- fun %in% names(data)
  }
  which(vars_lgl)
}

get_fallback_qm <- function(){
  all_calls <- as.character(lapply(sys.calls(), `[[`, 1))
  funs <- c("summarize","summarize", "mutate", "transmute", "select", "gather")
  funs_lgl <- funs %in% rev(all_calls)
  if(!any(funs_lgl)) return(utils::`?`)

  # find in call stack the last of those functions that was called
  fun <- funs[which.max(funs_lgl)]
  switch(fun,
         summarize=,
         summarise = qm_summarize_splice,
         transmute=,
         mutate = qm_mutate_splice,
         gather=,
         select= qm_select_gather
  )
}
