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

get_counts <- function (number_of_genes,
                        numbers_of_changed_genes,
                        number_of_replicates_per_condition) {

  profiles <- local({
    baseline <- get_baseline(number_of_genes)
    fold_change <- 2
    sapply(
            random_tiles(number_of_genes,
                         numbers_of_changed_genes),
            function (tile) {
              baseline[tile] <- baseline[tile] * fold_change
              baseline
            },
            simplify = FALSE
          )
  })

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

  setNames(
            data.frame(
                        get_bare_counts(profiles, expected_library_sizes),
                        row.names = indexed_names("GENE", number_of_genes)
                      ),
            indexed_names("SAMPLE",
                          length(numbers_of_changed_genes) *
                          number_of_replicates_per_condition)
          )
}

## ----------------------------------------------------------------------------

get_bare_counts <- function (profiles, expected_library_sizes) {

  lambda <- do.call(
                     cbind,
                     mapply(
                             function (profile_, sizes) {
                               as.matrix(profile_) %*% t(as.matrix(sizes))
                             },
                             profiles,
                             expected_library_sizes,
                             SIMPLIFY = FALSE
                           )
                   )

  number_of_genes <- nrow(lambda)
  number_of_samples <- ncol(lambda)

  # biological variation
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

  # technical variation
  matrix(rpois(number_of_genes * number_of_samples,
               lambda = mu),
         number_of_genes,
         number_of_samples)
}

## ---------------------------------------------------------------------

get_baseline <- local({

  # load(url("http://bioinf.wehi.edu.au/voom/qAbundanceDist.RData"))
  load("/home/berriz/_/projects/rnaseq/simulation/src/qAbundanceDist.RData")

  function (number_of_genes) {
    raw <- qAbundanceDist((1:number_of_genes) / (number_of_genes + 1))
    raw / sum(raw)
  }
})

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

indexed_names <- function (prefix, n) {
  format <- sprintf("%s_%%0%dd", prefix, floor(log10(n)) + 1)
  sapply(1:n, function (i) sprintf(format, i))
}

## ----------------------------------------------------------------------------

main <- function () {

  numbers_of_changed_genes <- c(4, 6)
  number_of_replicates_per_condition <- 3

  number_of_genes <- local({
    fraction_of_changed_genes <- 0.02
    round(sum(numbers_of_changed_genes) / fraction_of_changed_genes)
  })

  ## --------------------------------------------------------------------------

  set.seed(1)

  all_counts <- get_counts(number_of_genes,
                           numbers_of_changed_genes,
                           number_of_replicates_per_condition)

  browser()

  # Filter
  counts_threshold <- 10
  all_counts[rowSums(all_counts) >= counts_threshold, ]
}

main()

################################################################################
################################################################################

## function () {

##   ## ---------------------------------------------------------------------
##   # load(url("http://bioinf.wehi.edu.au/voom/qAbundanceDist.RData"))
##   load("/home/berriz/_/projects/rnaseq/simulation/_scratch/qAbundanceDist.RData")

##   baseline <- local({
##     raw <- qAbundanceDist((1:number_of_genes)/(number_of_genes + 1))
##     raw/sum(raw)
##   })

##   baseline1 <- baseline2 <- baseline
##   ## browser()

##   local({
##     fold_change <- 2
##     baseline1[i_1] <<- baseline1[i_1] * fold_change
##     baseline2[i_2] <<- baseline2[i_2] * fold_change
##   })

##   ## ---------------------------------------------------------------------

##   ## number_of_conditions <- 2
##   ## number_of_replicates_per_condition <- 3
##   ## number_of_samples <- (number_of_conditions *
##   ##                       number_of_replicates_per_condition)

##   ## indices_of_changed_genes <- sample(1:number_of_genes,
##   ##                                    number_of_changed_genes)

##   ## get_baseline_for_condition <- function (condition_index,
##   ##                                         fold_change) {
##   ##   start_ <- 1 + (condition_index - 1) * number_of_replicates_per_condition
##   ##   end_   <- start_ + number_of_replicates_per_condition - 1
##   ##   baseline_for_condition <- baseline
##   ##   indices_to_change <- indices_of_changed_genes[start_:end_]
##   ##   baseline_for_condition[indices_to_change] <-
##   ##     baseline_for_condition[indices_to_change] * fold_change
##   ##   baseline_for_condition
##   ## }

##   ## get_baseline_for_condition(1, fold_change = 2)

##   n1 <- 3
##   n2 <- 3
##   number_of_samples <- n1 + n2

##   expected_library_size <- local({

##     # the value for expected_number_of_counts_per_gene is derived from the
##     # the values used the voom code
##     expected_number_of_counts_per_gene <- 1100

##     expected_number_of_counts_per_gene * number_of_genes

##   })

##   lambda <- local({

##     # the value for expected_number_of_counts_per_gene is derived from the
##     # the values used the voom code
##     expected_number_of_counts_per_gene <- 1100

##     expected_library_size <- rep(expected_number_of_counts_per_gene * number_of_genes,
##                                  number_of_samples)

##     browser()

##     cbind(
##       as.matrix(baseline1) %*% t(as.matrix(expected_library_size[1:n1])),
##       as.matrix(baseline2) %*% t(as.matrix(expected_library_size[(n1 + 1):(n1 + n2)]))
##     )

##   })
##   ## dim(lambda) == c(number_of_genes, number_of_samples)

##   counts <- get_counts(lambda)
## }
