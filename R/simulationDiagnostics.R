#' Shiny UI module for simulation diagnostic results
#'
#' @param id Namespace identifier
#'
#'
#' @return rhandsontable with SamplingStrata input parameters
#'
#' @export
simuDiagnosticUI<-function(id) {
  ns<-NS(id)
  tagList(
    fluidRow(
      column(4,
             h4("Overall CV", style = "text-align: center;"),
             plotly::plotlyOutput(ns("cv_plot"), width = 200, height = 200)
      ),
      column(4,
             h4("Overall Bias", style = "text-align: center;"),
             plotly::plotlyOutput(ns("rel_bias"), width = 200, height = 200)
      ),
      column(4,
             h4("Simulation Results", style = "text-align: center;"),
             DT::dataTableOutput(ns("design_summary"))
      )
    )
  )
}

#' Shiny server module for simulation diagnostic results
#'
#' @description This module creates the outputs for the results from a sampling
#' simulation. The frame must contain at least the columns \emph{Mean, SE, CV, bias}. In cases of domain
#' or stratified sampling, a 5th column may be included named \emph{Domain}.
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param dataset is the dataset from the simulation output, must be a \emph{data.table}
#'
#' @export
#'
#'

simuDiagnosticSRV<-function(input, output, session, dataset) {
  ## styles
  smTab<-list(dom="tp")

  output$cv_plot<-plotly::renderPlotly({
    final<<-dataset
    req(final)
    if(length(final)==4) {
      plotly::plot_ly(final, y = ~CV, type = "box", name = " ", color = "orange") %>%
        plotly::layout(yaxis = list(title = "CV",
                                    zeroline = TRUE,
                                    range = c(min(final$CV), max(final$CV))),
                       xaxis = list(title = "Y1"))
    } else if(length(final)==5) {
      plotly::plot_ly(final, y = ~CV, x = ~Domain, type = "box", name = " ", colors = "orange") %>%
        plotly::layout(yaxis = list(title = "CV",
                                    zeroline = TRUE,
                                    range = c(min(final$CV), max(final$CV))),
                       xaxis = list(title = sprintf("Y%d", 1:dplyr::n_distinct(final$Domain))))
    }
  })

  output$rel_bias<-plotly::renderPlotly({
    final<-dataset
    req(final)
    ql<-stats::quantile(final$bias,0.05)
    qu<-stats::quantile(final$bias,0.95)
    final[,bias:=ifelse(bias<=ql, ql, bias)][,bias:=ifelse(bias>=qu, qu, bias)]
    if(length(final)==4) {
    plotly::plot_ly(final, y = ~abs(bias), type = "box", name = " ", color = "blue") %>%
      plotly::layout(yaxis = list(title = "Rel. Bias",
                                  zeroline = TRUE,
                                  range = c(min(abs(final$bias)), max(abs(final$bias)))),
                     xaxis = list(title = "Y1"))
    } else if(length(final)==5) {
      plotly::plot_ly(final, y = ~abs(bias), x = ~Domain, type = "box", name = " ", colors = "blue") %>%
        plotly::layout(yaxis = list(title = "Rel. Bias",
                                    zeroline = TRUE,
                                    range = c(min(abs(final$bias)), max(abs(final$bias)))),
                       xaxis = list(title = sprintf("Y%d", 1:dplyr::n_distinct(final$Domain))))

    }

  })
  ## Summary table
  output$design_summary<-DT::renderDataTable({
    tab<-copy(dataset)
    req(tab)
    DT::datatable(tab, rownames = F, options = smTab) %>%
      DT::formatRound(1:length(tab), 2)
  })
}
