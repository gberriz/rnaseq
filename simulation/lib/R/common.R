## ----------------------------------------------------------------------------

import("misc")

## ----------------------------------------------------------------------------

read_expected_fold_changes <- function (expected_fold_changes_dir) {

  misc$collect(expected_fold_changes_dir,
               header = FALSE,
               col.names = c("gene_id", "expected_fold_change"),
               row.names = "gene_id")


}

## ----------------------------------------------------------------------------
