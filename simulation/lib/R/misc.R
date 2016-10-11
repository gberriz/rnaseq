## ----------------------------------------------------------------------------

mkdir_for <- function (path) {
  dir.create(dirname(path),
             showWarnings = FALSE,
             recursive = TRUE,
             mode = "0775")
}

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

read_expected_fold_changes <- function (expected_fold_changes_dir) {

  collect(expected_fold_changes_dir,
          header = FALSE,
          col.names = c("gene_id", "expected_fold_change"),
          row.names = "gene_id")


}
