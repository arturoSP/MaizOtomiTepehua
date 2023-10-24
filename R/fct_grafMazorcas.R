#' grafMazorcas
#'
#' @description A fct function
#'
#' @return An interactive plot.
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_longer
#' @import echarts4r
#'
#' @noRd

plotMazorca <- function(){
  dataOT <- bdMaizOT[,c(1:4, 6, 7, 9, 24, 25)]
  mazorcaCuanti <- dataOT |>
    dplyr::select(-c(8,9)) |>
    tidyr::pivot_longer(cols = c(HilerasMazorca:PesoMazorca),
                        names_to = "Metrica", values_to = "Valor") |>
    dplyr::filter(!is.na(Valor)) |>
    dplyr::mutate(Metrica = factor(Metrica,
                            levels = c("HilerasMazorca", "LongitudMazorca",
                                       "DiametroMazorca", "PesoMazorca"),
                            labels = c("Hileras por\nmazorca", "Longitud [cm]",
                                       "Diámetro [cm]", "Peso [g]"),
                            ordered = T))

 p1 <- mazorcaCuanti |>
    dplyr::group_by(Metrica) |>
    echarts4r::e_chart(Localidad, timeline = TRUE) |>
    echarts4r::e_color(background = "#fffce2") |>
    echarts4r::e_scatter(Valor, colorBy = 'data',
                         symbol_size = 20,
                         legend = FALSE) |>
   echarts4r::e_toolbox_feature('saveAsImage') |>
   echarts4r::e_timeline_serie(title = list(
     list(text = "Hileras por mazorca"),
     list(text = "Longitud de la mazorca [cm]"),
     list(text = "Diámetro de la mazorca [cm]"),
     list(text = "Peso de la mazorca [g]")
   )) |>
   echarts4r::e_tooltip(trigger = "item")

 return(p1)
}
