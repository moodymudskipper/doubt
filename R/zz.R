
#' @importFrom stats ave
NULL


# nocov start
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.doubt <- list(
    doubt.registered_ops = character(0),
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
# nocov end

# for development with devtools, as devtools masks `?` and we don't want
# it in our global env
# doubt0 <- list(`?` = doubt::`?`)
# attach(doubt0)

