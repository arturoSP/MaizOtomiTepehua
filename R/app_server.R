#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom echarts4r renderEcharts4r
#' @importFrom leaflet renderLeaflet
#' @importFrom slickR renderSlickR
#' @noRd
#'

app_server <- function(input, output, session) {

  output$grafMDS <- plotly::renderPlotly(plotMDS()) #echarts4r::renderEcharts4r(plotMDS())

  output$grafGranos1 <- echarts4r::renderEcharts4r(plotGrano()[[1]])

  output$grafGranos2 <- echarts4r::renderEcharts4r(plotGrano()[[2]])

  output$grafMazorcas <- echarts4r::renderEcharts4r(plotMazorca())

  output$grafMapa <- leaflet::renderLeaflet(plotMap())

  output$carousel <- slickR::renderSlickR(doCarousel())

  output$grafProductores <- echarts4r::renderEcharts4r(plotProductores()[[1]])

  output$grafMilpa <- echarts4r::renderEcharts4r(plotProductores()[[2]])

  output$grafPreparacion <- echarts4r::renderEcharts4r(plotProductores()[[3]])

  output$grafUsos <- echarts4r::renderEcharts4r(plotProductores()[[5]])

  output$grafAlimentos <- echarts4r::renderEcharts4r(plotProductores()[[4]])

  output$grafFechas <- echarts4r::renderEcharts4r(plotProductores()[[6]])

  output$grafProblemas <- echarts4r::renderEcharts4r(plotProductores()[[7]])
}
