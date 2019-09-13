# 1) for general version precedence_ops should be a list of symboles,
# 2) then we should count ++ and instead of startsWith
# 3) then subset list of symbols to take the right one

#' count_plus("+a")
#' count_plus("+++b")
count_plus <- function(x){
  attr(gregexpr("^\\++", x)[[1]],"match.length")
}

build_placeholder_fun <- function(fun_chr, precedence_op){
  body <- substitute({
    sc <- sys.call()
    # last_arg is the rhs if binary or the rhs if unary
    last_arg <- deparse(sc[[length(sc)]])

    if(startsWith(last_arg, "+")){
      # remove the 1st character
      last_arg <- substr(last_arg, 2, nchar(last_arg))
      # parse
      last_arg <- str2lang(last_arg)
      # replace the original argument
      sc[[length(sc)]] <- last_arg
      # replace the placeholder operator by the right function
      sc[[1]] <- quote(op)
      # evaluate expression with modified parse tree in initial environment
      return(eval.parent(sc))
    }
    sc[[1]] <- quote(base::precedence_op)
    return(eval.parent(sc))
  }, list(
    op = as.symbol(fun_chr),
    precedence_op = as.symbol(precedence_op)))

  fun <- as.function(c(alist(e1=, e2=), body))
}

build_placeholder_fun2 <- function(funs_chr, precedence_op){
  body <- substitute({
    sc <- sys.call()
    # last_arg is the rhs if binary or the rhs if unary
    last_arg <- deparse(sc[[length(sc)]])

    n_plus <- count_plus(last_arg)
    if(n_plus > 0){
      # remove the + characters
      last_arg <- substr(last_arg, n_plus+1, nchar(last_arg))
      # parse
      last_arg <- str2lang(last_arg)
      # replace the original argument
      sc[[length(sc)]] <- last_arg
      # replace the placeholder operator by the right function
      sc[[1]] <- str2lang(ops[[n_plus]])
      # evaluate expression with modified parse tree in initial environment
      return(eval.parent(sc))
    }
    sc[[1]] <- quote(base::precedence_op)
    return(eval.parent(sc))
  }, list(
    ops = funs_chr,
    precedence_op = as.symbol(precedence_op)))

  fun <- as.function(c(alist(e1=, e2=), body))
}
