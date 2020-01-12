# 1) for general version precedence_ops should be a list of symboles,
# 2) then we should count ++ and instead of startsWith
# 3) then subset list of symbols to take the right one

count_plus <- function(x){
  attr(gregexpr("^-\\++-", x)[[1]],"match.length") - 2
}

build_placeholder_fun <- function(funs_chr, precedence_op){
  body <- substitute({
    sc <- as.list(sys.call())
    if(length(sc) == 3 && substr(deparse_rec(sc[[3]]),1,2) != "-+"){
      # We don't find a `-` sign on the rhs so we replace the op by
      # the standard version and reevaluate in parent
      sc[[1]] <- as.call(expression(`::`,base,precedence_op)) #quote(getFromNamespace(precedence_op, "base"))
      # print(as.call(sc))
      return(eval.parent(as.call(sc)))
    }
    # last_arg is the rhs if binary or the rhs if unary
    last_arg <- deparse(sc[[length(sc)]])

    n_plus <- count_plus(last_arg[1])
    # remove the + characters
    last_arg[1] <- substr(last_arg[1], n_plus+3, nchar(last_arg))
    last_arg <- paste(last_arg, collapse = "\n")
    # parse
    last_arg <- str2lang(last_arg)
    if(is.call(last_arg) && identical(last_arg[[1]], quote(`{`))){
      last_arg[[1L]] <- NULL
      nms <- allNames(last_arg)
      for(i in seq_along(last_arg)) {
        arg <- last_arg[[i]]
        if(is.call(arg) && identical(arg[[1L]], quote(`=`))) {
          if(!is.symbol(nm <- arg[[2L]])) stop("Invalid argument name: ", deparse(arg[[2L]]),call. = FALSE)
          nms[i] <- as.character(nm)
          if(is.null(arg[[3L]])) last_arg[i] <- list(NULL) else
          last_arg[[i]] <- arg[[3L]]
        }
      }
      names(last_arg) <- nms
    }


    sc[[1]] <- str2lang(ops[[n_plus]])
    # replace the original argument
    sc <- as.call(c(sc[-length(sc)],last_arg))

    # evaluate expression with modified parse tree in initial environment
    eval.parent(sc)
  }, list(
    ops = funs_chr,
    precedence_op = as.symbol(precedence_op)))

  fun <- as.function(c(alist(e1=, e2=), body))
}
