
build_op_qm_call_and_ops <- function(txt, pattern) {
  m_all <- gregexpr(pattern, txt, perl = TRUE)[[1]]
  capture_matrix <- matrix(ncol=3, substring(
      txt,
      attr(m_all, "capture.start"),
      attr(m_all, "capture.start") + attr(m_all, "capture.length") - 1
    ))
  # remove duplicate rows
  capture_matrix <- unique(capture_matrix)
  # all captured function names
  fun_chr_all <- capture_matrix[,3]

  funs_exist_lgl <- sapply(fun_chr_all, existsFunction)
  # the dot always "exists"
  funs_exist_lgl[names(funs_exist_lgl) == "."] <- TRUE
  if(!all(funs_exist_lgl))
    stop(sprintf("no function named '%s' was found",
                 fun_chr_all[!funs_exist_lgl]))
  dubious_op_all    <- capture_matrix[,1] # e.g %%foo?
  precedence_op_all <- capture_matrix[,2] # e.g. %%

  # number the instances of distinct precedence ops
  indices <- ave(seq_along(precedence_op_all), precedence_op_all, FUN = seq_along)

  # e.g. replace "%%foo?" by "%%++" (with relevant number of `+`)
  for (i in seq_len(nrow(capture_matrix))){
    txt <- gsub(regex.escape(dubious_op_all[[i]]),
                # space necessary here in case previous character is
                paste0(precedence_op_all[[i]]," -",strrep("+", indices[[i]]),"-"),
                txt)
  }

  call_all <- str2lang(txt)

  fun_chr_all[fun_chr_all == "."] <- "doubt:::pipe"
  fun_lists <- split(fun_chr_all, precedence_op_all)
  #print(fun_lists)
  ops <- Map(build_placeholder_fun, fun_lists, names(fun_lists))
  list(call = call_all, ops = ops)
}

pipe <- function(lhs, rhs) {
    eval(substitute(rhs), envir = list(. = lhs), enclos = parent.frame())
}

# build_op_qm_call_and_1op <- function(txt, op_qm_pattern){
#   m <- gregexpr(op_qm_pattern, txt, perl = TRUE)[[1]]
#   # cs like captured string
#
#   cs <- substring(txt, attr(m, "capture.start") , attr(
#     m, "capture.start") + attr(m, "capture.length") - 1)
#   fun_chr    <- cs[[3]] # e.g. foo
#   op <- get0(fun_chr)
#   if(is.null(op)) stop(sprintf("no function named '%s' was found", fun_chr))
#   #cs[1:2] <- regex.escape(cs[1:2])
#   dubious_op    <- cs[[1]] # e.g %%foo?
#   precedence_op <- cs[[2]] # e.g. %%
#
#   # e.g. replace %%foo? by %%+
#   txt <- gsub(regex.escape(dubious_op), paste0(precedence_op,"+"), txt)
#   # parse it
#   call <- str2lang(txt)
#   fun <- build_placeholder_fun(fun_chr, precedence_op)
#   op <- setNames(list(fun), precedence_op)
#   list(call = call, op = op)
# }

# build_double_qm_call <- function(txt, double_qm_pattern){
#   fun_chr <- gsub(double_qm_pattern, "\\2", txt)
#   op <- get0(fun_chr)
#   if(is.null(op)) stop(sprintf("no function named '%s' was found", fun_chr))
#
#   lhs_rhs <- strsplit(txt, paste0("\\?",fun_chr,"\\?"))[[1]]
#   args_chr <- unlist(strsplit(lhs_rhs,"?", TRUE))
#   args <- parse(text= args_chr)
#   call <- as.call(c(as.symbol(fun_chr),args))
# }
