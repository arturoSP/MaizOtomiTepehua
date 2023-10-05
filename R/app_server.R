#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import echarts4r
#' @noRd
#'

app_server <- function(input, output, session) {

  output$grafMDS <- plotly::renderPlotly(plotMDS()) #echarts4r::renderEcharts4r(plotMDS())

  output$grafGranos1 <- echarts4r::renderEcharts4r(plotGrano()[[1]])

  output$grafGranos2 <- echarts4r::renderEcharts4r(plotGrano()[[2]])

  output$grafMazorcas <- echarts4r::renderEcharts4r(plotMazorca())
}
