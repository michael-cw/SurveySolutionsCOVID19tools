% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samplingCube_selection.R
\name{samplingCubeOutput}
\alias{samplingCubeOutput}
\title{Shiny server module for sampling::samplecube balancing variable selection}
\usage{
samplingCubeOutput(input, output, session, dataset)
}
\arguments{
\item{input}{standard shiny input}

\item{output}{standard shiny output}

\item{session}{standard shiny session object}

\item{dataset}{is the dataset provided by the user}
}
\description{
This module creates the inputs and also updates them after dataset is
available. Due to the construction of this module, the inputs are availble in the
global environment under input$[id-ns(inputs)]. This means to reference i.e to
the selectizeInput ns("strat_var_cont) with the hypotetical id "stratInputs" you have
to use input$`stratInputs-strat_var_cont`.
}
