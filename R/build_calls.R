
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
