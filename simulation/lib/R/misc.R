## ----------------------------------------------------------------------------

mkdir <- function (path, mode = "0775") {
  dir.create(path,
             showWarnings = FALSE,
             recursive = TRUE,
             mode = mode)
}

mkdir_for <- function (path, ...) mkdir(dirname(path), ...)

## ----------------------------------------------------------------------------

write_2d_data <- function (
                           dataframe_or_matrix,
                           output_file,
                           row_names = TRUE,
                           column_names = TRUE
                          ) {

  parameters = list(
                    NULL,
                    file = output_file,
                    quote = FALSE,
                    sep = "\t",
                    col.names = column_names
                   )

  if (is.character(row_names)) {

    parameters[[1]] <- setNames(data.frame(row.names(dataframe_or_matrix),
                                           dataframe_or_matrix),
                                c(row_names,
                                  colnames(dataframe_or_matrix)))
    parameters$row.names <- FALSE
  }
  else {
    parameters[[1]] <- dataframe_or_matrix
    parameters$row.names <- row_names
  }

  mkdir_for(output_file)

  do.call(write.table, parameters)
}

## ----------------------------------------------------------------------------

collect <- function (inputdir, basename_ = "data.tsv", ...) {

  default_arguments <- list(header = TRUE)

  arguments <- modifyList(default_arguments, list(...))

  collect_ <- function (inputdir) {
    inputfile <- file.path(inputdir, basename_)
    if (file.exists(inputfile)) {
      do.call(read.table,
              c(list(inputfile), arguments))
    }
    else {
      sapply(
              list.files(path = inputdir),
              function (subdir) {
                collect_(file.path(inputdir, subdir))
              },
              simplify = FALSE
            )
    }
  }

  collect_(inputdir)
}

## ----------------------------------------------------------------------------

seq_along_by <- function(x, by = 1L) {

  1L + seq(from = 0, to = (length(x) - 1) %/% by) * by

}

## ----------------------------------------------------------------------------

cond <- function (...) {

  parent_environment <- parent.frame()

  cond_ <- function (arguments) {

    for (i in seq_along_by(arguments, by = 2)) {

      if (eval(arguments[[i]], envir = parent_environment))
        return(eval(arguments[[i + 1]], envir = parent_environment))

    }

  }

  arguments <- match.call(expand.dots = FALSE)$`...`

  if (length(arguments) %% 2 == 1) {

    cond_(c(head(arguments, n = -1L),
            list("TRUE"),
            tail(arguments, n = 1L)))

  }
  else {

    cond_(arguments)

  }

}

## ----------------------------------------------------------------------------

import_from_environment <- function (
                                      environment_,
                                      wanted,
                                      target = parent.frame()
                                    ) {

  list2env(as.list(environment_)[wanted], envir = target)

}

## ----------------------------------------------------------------------------

read_metadata <- function (metadatadir, wanted_columns) {

  full_table <- local({

    arguments <- list(file = NULL,
                      colClasses = "character",
                      row.names = "symbol")

    table_file <- file.path(metadatadir, "data.tsv")

    if (file.exists(table_file)) {
      arguments$file <- table_file
      arguments$header <- TRUE
    }
    else {
      headers_file <- file.path(metadatadir, "headers.tsv")
      rows_file <- file.path(metadatadir, "rows.tsv")

      if (file.exists(headers_file) && file.exists(rows_file)) {
        arguments$file <- rows_file
        headers <- scan(
                         headers_file,
                         what = "character",
                         quiet = TRUE
                       )
        arguments$header <- FALSE
        arguments$col.names <- headers
      }
    }

    do.call(read.table, arguments)

  })

  if (missing(wanted_columns)) return(full_table)

  raw <- subset(full_table, select = names(wanted_columns))

  ordered_factor <- function (values) {
    factor(values, levels = unique(values), ordered = TRUE)
  }

  process_column <- function (column, type) {
    switch(type,
           logical = as.logical(column),
           ordered = ordered_factor(column),
                     column) # default
  }

  data.frame(
              mapply(process_column,
                     as.list(raw),
                     wanted_columns[names(raw)],
                     SIMPLIFY = FALSE),
              row.names = row.names(raw)
            )

}

## ----------------------------------------------------------------------------

path_to_script <- function () {

  leading_command_arguments <- local({

    all_command_arguments <- commandArgs(trailingOnly = FALSE)

    max_index <-
      length(all_command_arguments) - length(commandArgs(trailingOnly = TRUE))

    all_command_arguments[1:max_index]

  })

  paths <- local({

    # the gsub statement below maps all elements of leading_command_arguments
    # that match "--file=<PATH>" to "<PATH>", and the rest to "".  For example,
    # if leading_command_arguments is
    #
    # c("/usr/lib/R/bin/exec/R", "--file=path/to/script.R", "--slave", "--no-restore", "--args")
    #
    # ...then paths_and_empties will be
    #
    # c("", "path/to/script.R", "", "", "")

    paths_and_empties <- gsub("^(?:--file=(.*)|.*)$", "\\1",
                                 leading_command_arguments)

    paths_and_empties[paths_and_empties != ""]

  })

  # if multiple --file arguments are given, R uses the last one
  tail(paths, 1)
}

## ----------------------------------------------------------------------------

warn <- function (...) cat(sprintf(...), file = stderr())

die <- function (status = 1, ...) {
  warn(...)
  ## FIXME: use exit
  ## exit(1)
  quit(save = "no", status = status)
}

exit <- function (status) quit(save = "no", status = status)

## ----------------------------------------------------------------------------

make_usage <- function (template, name) {
  noname <- missing(name)
  function (status = 1) {
    this <- if (noname) path_to_script() else name
    warn(sprintf("Usage:\n%s\n", gsub('%s', this, template)))
    exit(status)
  }
}

get_arguments <- function (argument_names, usage, exact = TRUE) {

  received_arguments <- as.list(commandArgs(trailingOnly = TRUE))

  number_received <- length(received_arguments)
  number_expected <- length(argument_names)
  number_missing <- number_expected - number_received

  if ((exact && number_missing != 0) ||
                number_missing  < 0) {
    usage(ifelse(number_of_received_arguments == 0, 0, 1))
  }

  setNames(c(as.list(received_arguments), as.list(rep(NA, number_missing))),
           argument_names)

}

## ----------------------------------------------------------------------------

is_empty <- function (set) length(set) == 0

## FIXME: is_subset should use is_empty
## is_subset <- function (set, otherset) is_empty(setdiff(set, otherset))
is_subset <- function (set, otherset) length(setdiff(set, otherset)) == 0

## FIXME: is_superset should use is_subset
## is_superset <- function (set, otherset) is_subset(otherset, set)
is_superset <- function (set, otherset) length(setdiff(otherset, set)) == 0

## ----------------------------------------------------------------------------

as_boolean <- function (anything) {
  string_value <- suppressWarnings(as.character(anything))
  if (identical(string_value, "")) return(FALSE)

  numeric_value <- suppressWarnings(as.numeric(as.logical(anything)))
  if (identical(numeric_value, 0)) FALSE else TRUE
}

## ----------------------------------------------------------------------------
