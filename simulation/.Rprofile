import <- function (
                    module_name,
                    as = NULL,
                    library_path = Sys.getenv("R_MODULES"),
                    target = parent.frame()
                   ) {

  ## usage:
  ## import("foo", as = "oof")
  ## oof$bar()
  ## oof$baz(1, 5, -1)

  module_path <- file.path(library_path, paste0(module_name, ".R"))

  if (!file.exists(module_path)) {
    stop(sprintf('import: file "%s" does not exist', module_path))
  }

  module <- new.env()

  source(module_path, local = module)

  alias <- ifelse(is.null(as), module_name, as)

  list2env(setNames(list(module), list(alias)), envir = target)

  invisible()

}
