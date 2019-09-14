#' @export
register_ops <- function(ops){
  registered_ops <- unlist(getOption("doubt.registered_ops"))
  # combine and sort starting with longest to avoid ambiguity
  all_ops <- union(registered_ops, ops)
  all_ops <- all_ops[order(nchar(all_ops),decreasing = TRUE)]
  options(doubt.registered_ops = all_ops)
}
