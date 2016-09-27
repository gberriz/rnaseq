## From the voom paper:
##
##   Six RNA-seq count libraries were simulated with counts for 10,000
##   genes. The first three libraries were treated as group 1 and the
##   others as group 2. The distribution of cpm [counts-per-million]
##   values for each library was simulated to match the distribution
##   that we observed for a real RNA-seq dataset from our own
##   practice.  The NB [negative binomial] dispersion ϕ was set to
##   decrease on average with expected count size, asymptoting to 0.2
##   squared for large counts. This degree of biological variation is
##   representative of what we observe for real RNA-seq data, being
##   larger than we typically observe between genetically identical
##   laboratory mice but less than we typically see between unrelated
##   human subjects (Figure 1). An individual dispersion ϕ was
##   generated for each gene around the trend according to an inverse
##   chi-square distribution with 40 degrees of freedom.
##
##   ...
##
##   The distribution of baseline values was chosen to match that from
##   RNA-seq experiments conducted at our institution. Specifically we
##   used the goodTuringProportions function of the edgeR package
##   [12], which implements the Good-Turing algorithm [55], to predict
##   the true proportion of total RNA attributable to each gene.  We
##   ran this function on a number of different libraries, pooled the
##   predicted proportions and formed a smoothed distribution
##   function. The baseline proportions for the simulations were then
##   generated to follow this distribution.
##
##   ...
##
##   The NB dispersions were generated as follows. The trend in the
##   dispersions was set to be ψ_gi with:
##
##   sqrt(ψ_gi) = 0.2 + 1/sqrt(λ_gi)
##
##   where λ_gi is the expected count size. A modest amount of
##   gene-wise biological variation was generated from an inverse
##   chi-square distribution with 40 degrees of freedom. The
##   individual dispersions were set to be ϕ_gi = ψ_gi * δ_g where
##   40/δ_g ∼ (χ_40)^2.

## ----------------------------------------------------------------------------

get_counts <- function (fold_changes, expected_library_sizes) {

  lambda <- get_expected_counts(fold_changes, expected_library_sizes)

  number_of_genes <- nrow(lambda)
  number_of_samples <- ncol(lambda)

  ## --------------------------------------------------------------------------
  ## biological variation
  phi <- local({

    ## From the voom paper:
    ##
    ##   The NB dispersions were generated as follows. The trend in the
    ##   dispersions was set to be ψ_gi with:
    ##
    ##   ψ_gi^(1/2) = 0.2 + (λ_gi)^(-1/2)
    ##
    ##   where λ_gi is the expected count size. A modest amount of
    ##   gene-wise biological variation was generated from an inverse
    ##   chi-square distribution with 40 degrees of freedom. The
    ##   individual dispersions were set to be ϕ_gi = ψ_gi * δ_g where
    ##   40/δ_g ∼ (χ_40)^2.

    psi <- (0.2 + 1/sqrt(lambda))^2
    ## dim(psi) == c(number_of_genes, number_of_samples)

    degrees_of_freedom <- 40
    chisq <- rchisq(number_of_genes, df = degrees_of_freedom)
    ## length(chisq) == number_of_genes

    psi * (degrees_of_freedom / chisq)

  })
  ## dim(phi) == c(number_of_genes, number_of_samples)

  ## browser()

  shape <- 1 / phi
  ## dim(shape) == c(number_of_genes, number_of_samples)

  mu <- matrix(
                rgamma(number_of_genes * number_of_samples,
                       shape = shape,
                       scale = lambda / shape),
                number_of_genes,
                number_of_samples
              )
  ## dim(mu) == c(number_of_genes, number_of_samples)

  ## --------------------------------------------------------------------------
  ## technical variation
  counts_matrix <- matrix(rpois(number_of_genes * number_of_samples,
                                lambda = mu),
                          number_of_genes,
                          number_of_samples)

  to_counts_dataframe(counts_matrix)

}

## ----------------------------------------------------------------------------

get_filtered_counts <- function (all_counts, counts_threshold) {

  all_counts[rowSums(all_counts) >= counts_threshold, ]

}

## ----------------------------------------------------------------------------

get_expected_counts <- function (fold_changes, expected_library_sizes) {

  do.call(
           cbind,
           mapply(
                   function (profile_, sizes) {
                     as.matrix(profile_) %*% t(as.matrix(sizes))
                   },
                   fold_changes_to_profiles(fold_changes),
                   expected_library_sizes,
                   SIMPLIFY = FALSE
                 )
         )
}

## ----------------------------------------------------------------------------

get_baseline <- local({

  # load(url("http://bioinf.wehi.edu.au/voom/qAbundanceDist.RData"))
  load("/home/berriz/_/projects/rnaseq/simulation/src/qAbundanceDist.RData")

  function (number_of_genes) {
    raw <- qAbundanceDist((1:number_of_genes) / (number_of_genes + 1))
    raw / sum(raw)
  }

})

## ----------------------------------------------------------------------------

fold_changes_to_profiles <- function (fold_changes) {

  number_of_genes <- length(fold_changes[[1]])
  baseline <- get_baseline(number_of_genes)

  sapply(
          fold_changes,
          function (fold_change) baseline * fold_change,
          simplify = FALSE
        )
}

## ----------------------------------------------------------------------------

tiles <- function (...) {
  tiles_ <- function (start_, arguments) {
    if (length(arguments) > 0) {
      end_ <- start_ + arguments[[1]] - 1
      c(list(start_:end_),
        tiles_(end_ + 1, arguments[-1]))
    }
  }

  tiles_(1, list(...))
}

random_tiles <- function (n, lengths) {
  i <- sample(1:n, sum(lengths))
  sapply(
          do.call(tiles, as.list(lengths)),
          function (t) i[t],
          simplify = FALSE
        )
}

## ----------------------------------------------------------------------------

indexed_names <- function (prefix, n) {
  format <- sprintf("%s_%%0%dd", prefix, floor(log10(n)) + 1)
  sapply(1:n, function (i) sprintf(format, i))
}

## ----------------------------------------------------------------------------

to_counts_dataframe <- function (counts_matrix) {

  row_names <- local({
    number_of_genes <- nrow(counts_matrix)
    indexed_names("GENE", number_of_genes)
  })

  column_names <- local({
    number_of_samples <- ncol(counts_matrix)
    indexed_names("SAMPLE", number_of_samples)
  })

  setNames(
            data.frame(counts_matrix, row.names = row_names),
            column_names
          )

}

## ----------------------------------------------------------------------------

import <- function (
                    environment_,
                    wanted,
                    target = parent.frame()
                   ) {

  list2env(as.list(environment_)[wanted], envir = target)

}

## ----------------------------------------------------------------------------

voom_usecase <- function () {

  set.seed(1)

  ## --------------------------------------------------------------------------

  numbers_of_changed_genes <- c(4, 6)
  number_of_replicates_per_condition <- 3

  number_of_genes <- local({
    fraction_of_changed_genes <- 0.02
    round(sum(numbers_of_changed_genes) / fraction_of_changed_genes)
  })

  ## --------------------------------------------------------------------------

  fold_changes <- local({
    base <- rep(1, number_of_genes)
    fold_change <- 2
    sapply(
            random_tiles(number_of_genes, numbers_of_changed_genes),
            function (tile) `[<-`(base, tile, fold_change),
            simplify = FALSE
          )
  })

  ## --------------------------------------------------------------------------

  expected_library_sizes <- local({
    # the value for expected_number_of_counts_per_gene is derived from the
    # the values used the voom code
    expected_number_of_counts_per_gene <- 1100

    expected_library_size <-
      expected_number_of_counts_per_gene * number_of_genes

    replicate(
               length(numbers_of_changed_genes),
               rep(
                    expected_library_size,
                    number_of_replicates_per_condition
                  ),
               simplify = FALSE
             )
  })

  ## --------------------------------------------------------------------------

  all_counts <- get_counts(fold_changes, expected_library_sizes)

  environment()
}

## ----------------------------------------------------------------------------

mh_usecase <- function () {

  set.seed(0)

  ## --------------------------------------------------------------------------

  number_of_genes <- 500
  expected_number_of_counts_per_gene <- 1000
  number_of_replicates_per_condition <- 3

  ## --------------------------------------------------------------------------

  fold_changes <- local({

    make_fold_change <- function (residues, fold_change_specs) {

      n <- length(residues)

      stopifnot(length(fold_change_specs) == n)

      fold_change <- rep(1, number_of_genes)

      if (n == 0) return(fold_change)

      stride <- 20
      j <- 1:number_of_genes
      for (i in seq_along(residues)) {
        k <- which((j %% stride) %in% residues[[i]])
        fold_change[k] <- fold_change_specs[[i]]
      }

      fold_change
    }

    list(
          make_fold_change(list(),
                           list()),

          make_fold_change(list(c(1, 2, 5), c(3, 7)),
                           list(         2,     0.5)),

          make_fold_change(list(c(1, 4),    c(3, 6), c(5)),
                           list(      2,        0.3,    3))
        )

  })

  ## --------------------------------------------------------------------------

  expected_library_sizes <- local({

    expected_library_size <-
      expected_number_of_counts_per_gene * number_of_genes

    number_of_conditions <- length(fold_changes)

    replicate(
               number_of_conditions,
               rep(
                    expected_library_size,
                    number_of_replicates_per_condition
                  ),
               simplify = FALSE
             )
  })

  ## --------------------------------------------------------------------------

  all_counts <- get_counts(fold_changes, expected_library_sizes)

  environment()
}

## ----------------------------------------------------------------------------

main <- function () {

  import(mh_usecase(),
         c("all_counts",
           "fold_changes",
           "expected_library_sizes"))

  counts <-
  get_filtered_counts(all_counts, counts_threshold = 10)

  browser()
  counts

}

## ----------------------------------------------------------------------------

main()
