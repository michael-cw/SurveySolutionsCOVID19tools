#' @title Survey Solutons COVID19 survey toolbox
#'
#'
#'
#' @description This package contains a set of tools for the effecient implementation of Computer Assisted
#' Telephone Interview (CATI) surveys. Each of the tools contains the corresponding GUI,
#' either written in vanilla shiny or in rmarkdown's flexdashboard. This is only a first
#' version, and frequent updates will follow during the weeks to come, so make sure to
#' check by.
#'
#' Currently implemented are:
#'
#' \itemize{
#'    \item Sampling
#'       \itemize{
#'          \item \href{https://github.com/barcaroli/SamplingStrata}{SamplingStrata}
#'          \item \href{https://github.com/cran/sampling}{cubeSample (package sampling)}
#'        }
#'    \item Quality Control
#'    \item Analysis and Visualization
#' }
#'
#' @examples \dontrun{
#' suso_covid19_samplingApp()
#' }
#'
#' @docType package
#' @name SurveySolutionsCOVID19tools
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "3.3.0")  {
  utils::globalVariables(c(
    "ID",
    "multicore",
    "i",
    "minStr",
    "DOM1",
    "STRATO",
    "SOLUZ",
    "STRATUM1",
    "DOMAINVALUE",
    "LABEL",
    "pik",
    "Variable",
    "n_distinct",
    "COST"
    ))
}
