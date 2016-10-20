## ----------------------------------------------------------------------------
import("misc")
import("common")

## ----------------------------------------------------------------------------

SETTINGS <- list(placeholder = NULL

  , VERBOSE = misc$as_boolean(Sys.getenv("VERBOSE"))
  , METHODS = c("deseq2", "edger")
)

## ----------------------------------------------------------------------------
PARAMETER_DEFAULTS <- list(

  common = local({

    workDir <- NULL                                 # working directory for the R session

    projectName <- "projectName"                    # name of the project
    author <- "Your name"                           # author of the statistical analysis/report

    targetFile <- "target.txt"                      # path to the design/target file
    rawDir <- "raw"                                 # path to the directory containing raw counts files
    featuresToRemove <- c("alignment_not_unique",   # names of the features to be removed
                          "ambiguous",              # (specific HTSeq-count information and rRNA for example)
                          "no_feature",             # NULL if no feature to remove
                          "not_aligned",
                          "too_low_aQual")

    varInt <- "group"                               # factor of interest
    condRef <- "WT"                                 # reference biological condition
    batch <- NULL                                   # blocking factor: NULL (default) or "batch" for example

    alpha <- 0.05                                   # threshold of statistical significance
    pAdjustMethod <- "BH"                           # p-value adjustment method: "BH" (default) or "BY"

    colors <- c("dodgerblue","firebrick1",          # vector of colors of each biological condition on the plots
                "MediumVioletRed","SpringGreen")

    environment()
  }),

  deseq2 = local({
    fitType <- "parametric"                         # mean-variance relationship: "parametric" (default) or "local"
    cooksCutoff <- TRUE                             # TRUE/FALSE to perform the outliers detection (default is TRUE)
    independentFiltering <- TRUE                    # TRUE/FALSE to perform independent filtering (default is TRUE)

    typeTrans <- "VST"                              # transformation for PCA/clustering: "VST" or "rlog"
    locfunc <- "median"                             # "median" (default) or "shorth" to estimate the size factors

    environment()
  }),

  edger = local({
    cpmCutoff <- 1                                  # counts-per-million cut-off to filter low counts
    gene.selection <- "pairwise"                    # selection of the features in MDSPlot
    normalizationMethod <- "TMM"                    # normalization method: "TMM" (default), "RLE" (DESeq) or "upperquartile"

    environment()
  })

)

load_parameters <- function (method, user_settings, target) {

  list2env(as.list(PARAMETER_DEFAULTS$common), envir = target)
  list2env(as.list(PARAMETER_DEFAULTS[[method]]), envir = target)
  list2env(user_settings, envir = target)

}

load_parameters_for_deseq2 <- function (user_settings, target = parent.frame()) {
  load_parameters("deseq2", user_settings, target)
}

load_parameters_for_edger <- function (user_settings, target = parent.frame()) {
  load_parameters("edger", user_settings, target)
}

run_sartools <- list(
  deseq2 = function (user_settings) {

    current_directory <- getwd()
    on.exit(setwd(current_directory))

    load_parameters_for_deseq2(user_settings)

    dir.create(workDir,
               showWarnings = FALSE,
               recursive = TRUE,
               mode = "0775")

    ## ----------------------------------------------------------------------------
    ## the rest of this function's code comes straight from
    ## template_script_DESeq2.r

    setwd(workDir)

    library(SARTools)

    # checking parameters
    checkParameters.DESeq2(projectName=projectName,author=author,targetFile=targetFile,
                           rawDir=rawDir,featuresToRemove=featuresToRemove,varInt=varInt,
                           condRef=condRef,batch=batch,fitType=fitType,cooksCutoff=cooksCutoff,
                           independentFiltering=independentFiltering,alpha=alpha,pAdjustMethod=pAdjustMethod,
                           typeTrans=typeTrans,locfunc=locfunc,colors=colors)

    # loading target file
    target <- loadTargetFile(targetFile=targetFile, varInt=varInt, condRef=condRef, batch=batch)

    # loading counts
    counts <- loadCountData(target=target, rawDir=rawDir, featuresToRemove=featuresToRemove)

    # description plots
    majSequences <- descriptionPlots(counts=counts, group=target[,varInt], col=colors)

    # analysis with DESeq2
    out.DESeq2 <- run.DESeq2(counts=counts, target=target, varInt=varInt, batch=batch,
                             locfunc=locfunc, fitType=fitType, pAdjustMethod=pAdjustMethod,
                             cooksCutoff=cooksCutoff, independentFiltering=independentFiltering, alpha=alpha)

    # PCA + clustering
    exploreCounts(object=out.DESeq2$dds, group=target[,varInt], typeTrans=typeTrans, col=colors)

    # summary of the analysis (boxplots, dispersions, diag size factors, export table, nDiffTotal, histograms, MA plot)
    summaryResults <- summarizeResults.DESeq2(out.DESeq2, group=target[,varInt], col=colors,
                                              independentFiltering=independentFiltering,
                                              cooksCutoff=cooksCutoff, alpha=alpha)

    # save image of the R session
    save.image(file=paste0(projectName, ".RData"))

    # generating HTML report
    writeReport.DESeq2(target=target, counts=counts, out.DESeq2=out.DESeq2, summaryResults=summaryResults,
                       majSequences=majSequences, workDir=workDir, projectName=projectName, author=author,
                       targetFile=targetFile, rawDir=rawDir, featuresToRemove=featuresToRemove, varInt=varInt,
                       condRef=condRef, batch=batch, fitType=fitType, cooksCutoff=cooksCutoff,
                       independentFiltering=independentFiltering, alpha=alpha, pAdjustMethod=pAdjustMethod,
                       typeTrans=typeTrans, locfunc=locfunc, colors=colors)

    out.DESeq2
  },

  edger = function (user_settings) {

    current_directory <- getwd()
    on.exit(setwd(current_directory))

    load_parameters_for_edger(user_settings)

    dir.create(workDir,
               showWarnings = FALSE,
               recursive = TRUE,
               mode = "0775")

    ## ----------------------------------------------------------------------------
    ## the rest of this function's code comes straight from
    ## template_script_edgeR.r (except where noted)

    setwd(workDir)
    library(SARTools)

    # checking parameters
    checkParameters.edgeR(projectName=projectName,author=author,targetFile=targetFile,
                          rawDir=rawDir,featuresToRemove=featuresToRemove,varInt=varInt,
                          condRef=condRef,batch=batch,alpha=alpha,pAdjustMethod=pAdjustMethod,
                          cpmCutoff=cpmCutoff,gene.selection=gene.selection,
                          normalizationMethod=normalizationMethod,colors=colors)

    # loading target file
    target <- loadTargetFile(targetFile=targetFile, varInt=varInt, condRef=condRef, batch=batch)

    # loading counts
    counts <- loadCountData(target=target, rawDir=rawDir, featuresToRemove=featuresToRemove)

    # description plots
    majSequences <- descriptionPlots(counts=counts, group=target[,varInt], col=colors)

    # edgeR analysis
    out.edgeR <- run.edgeR(counts=counts, target=target, varInt=varInt, condRef=condRef,
                           batch=batch, cpmCutoff=cpmCutoff, normalizationMethod=normalizationMethod,
                           pAdjustMethod=pAdjustMethod)

    # MDS + clustering
    exploreCounts(object=out.edgeR$dge, group=target[,varInt], gene.selection=gene.selection, col=colors)

    # summary of the analysis (boxplots, dispersions, export table, nDiffTotal, histograms, MA plot)
    summaryResults <- summarizeResults.edgeR(out.edgeR, group=target[,varInt], counts=counts, alpha=alpha, col=colors)

    # save image of the R session
    save.image(file=paste0(projectName, ".RData"))

    cpmCutoff <<- cpmCutoff   ## work-around bug in SARTools

    # generating HTML report
    writeReport.edgeR(target=target, counts=counts, out.edgeR=out.edgeR, summaryResults=summaryResults,
                      majSequences=majSequences, workDir=workDir, projectName=projectName, author=author,
                      targetFile=targetFile, rawDir=rawDir, featuresToRemove=featuresToRemove, varInt=varInt,
                      condRef=condRef, batch=batch, alpha=alpha, pAdjustMethod=pAdjustMethod, colors=colors,
                      gene.selection=gene.selection, normalizationMethod=normalizationMethod)

    out.edgeR
  }
)

## save_scatterplot_png <- function (x, y, output_file) {
##   misc$mkdir_for(output_file)
##   plot(x, y)
##   dev.copy(png, output_file)
##   dev.off()
## }

save_plot_png <- function (xx, yy, output_file) {

  dir.create(dirname(output_file),
             showWarnings = FALSE,
             recursive = TRUE,
             mode = "0775")

  xs <- sort(unique(xx))

  boxplot(sapply(xs,
                 function (x) yy[xx == x],
                 simplify = FALSE),
          at = log2(xs),
          names = xs,
          log = "y",
          ylim = c(0.15, 6),
          boxwex = 0.5)

  dev.copy(png, output_file)
  dev.off()
}

run <- function (inputdir, user_settings, resultsdir) {

  expected_fold_changes_dir <- file.path(inputdir, "expected_fold_changes")

  expected_fold_changes <-
    common$read_expected_fold_changes(expected_fold_changes_dir)

  methods <- c("deseq2", "edger")

  output <-
    mapply(function (sartools_runner, settings, method) {
             cat(method, '\n')

             if (SETTINGS$VERBOSE) {
               sartools_runner(settings)
             }
             else {
               sink("/dev/null");
               on.exit(sink());
               suppressWarnings(suppressMessages(sartools_runner(settings)))
             }

           },
           run_sartools,
           user_settings,
           methods)

  ## --------------------------------------------------------------------------

  do_plots <- function (method, results, fold_change_column) {

    for (i in c(1, 2)) {
      condition <- sprintf("T%d", i)
      contrast_name <- sprintf("%s_vs_U", condition)
      results_for_contrast <- results[[contrast_name]]
      y <- 2**results_for_contrast[, fold_change_column]
      x <- expected_fold_changes[[condition]][row.names(results_for_contrast), ]
      output_file <- file.path(resultsdir, method, "simulation", condition, "plot.png")
      save_plot_png(x, y, output_file)
    }

  }

  wanted_column <- list(deseq2 = "log2FoldChange",
                        edger = "logFC")

  for (method in methods) {
    do_plots(method, output[[method]]$results, wanted_column[[method]])
  }

}

main <- function () {

  use_case <- "mh"
  basedir <- file.path("io", use_case)

  current_directory <- getwd()
  on.exit(setwd(current_directory))
  setwd(basedir)

  ## resultsdir <- file.path("results", use_case, "simulation")

  methods <- c("deseq2", "edger")

  for (targetdir in list.files(".",
                               pattern = "target",
                               recursive = TRUE,
                               include.dirs = TRUE)) {

    inputdir <- dirname(targetdir)
    cat(file.path(basedir, inputdir), '\n')

    sartoolsdir <- file.path(inputdir, "sartools")

    user_settings <-
      sapply(methods,
             function (method) {
               path <- file.path(sartoolsdir,
                                 method,
                                 "parameters.yaml")
               yaml::yaml.load_file(path)
             },
             simplify = FALSE)

    resultsdir <- file.path(inputdir, "results")
    run(inputdir, user_settings, resultsdir)
  }

}

SETTINGS$VERBOSE <- TRUE

main()
