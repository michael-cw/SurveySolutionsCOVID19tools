#' Start Sampling Application
#'
#'
#' @description Running this function starts the
#' the COVID19 CATI Surveys
#' Sampling Application for household survey sub-sample
#' in your browser
#'
#' @param launch.browser if TRUE starts in your default browser, otherwise in your IDE viewer
#' @param quiet if TRUE start-up messages are not printed
#'
#' @import shiny
#' @import data.table
#' @import rhandsontable
#' @import SamplingStrata
#' @import flexdashboard
#' @importFrom stats complete.cases
#'
#' @export
suso_covid19_samplingApp<-function(launch.browser = T, quiet = T) {
  fp<-file.path('sampling', 'suso_sampling_covid19')
  appFlex <- system.file(fp, "sampling_main.Rmd", package = "SurveySolutionsCOVID19tools")
  if (appFlex == "") {
    stop("Could not find example directory. Try re-installing `SurveySolutionsCOVID19tools`.", call. = FALSE)
  }
  rmarkdown::run(appFlex, shiny_args = list(launch.browser, quiet))
}
