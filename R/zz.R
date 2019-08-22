.onLoad <- function(libname, pkgname) {
  op <- options()
  op.doubt <- list(
    doubt.registered_ops = list(),
    doubt.name = "Antoine Fabri",
    doubt.desc.author = "Antoine Fabri <antoine.fabri@gmail.com> [aut, cre]",
    doubt.desc.license = "GPL-3",
    doubt.desc.suggests = NULL,
    doubt.desc = list()
  )
  toset <- !(names(op.doubt) %in% names(op))
  if(any(toset)) options(op.doubt[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  current_ops <- getOption("doubt.registered_ops")
  package_ops <- c("?julia>", "?!julia>")
  options(doubt.registered_ops = union(current_ops, package_ops))
  invisible()
}
