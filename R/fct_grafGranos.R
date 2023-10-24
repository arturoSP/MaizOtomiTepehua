#' grafGranos1
#'
#' @description A fct function
#'
#' @return An interactive plot.
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr transmute
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr inner_join
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_sentence
#' @import echarts4r
#'
#' @noRd

plotGrano <- function(){
  dataOT <- bdMaizOT[,c(1:3, 5,11:14,17,18, 21, 22, 26, 27)]

  granoColor <- dataOT[,-c(4:13)] |>
    dplyr::filter(!is.na(ColorGrano))

  granoColor$`amarillo` <- stringr::str_detect(granoColor$ColorGrano, "amarillo \\(B\\)")
  granoColor$`amarillo claro` <- stringr::str_detect(granoColor$ColorGrano, "amarillo claro")
  granoColor$`amarillo medio` <- stringr::str_detect(granoColor$ColorGrano, "amarillo medio")
  granoColor$`amarillo naranja` <- stringr::str_detect(granoColor$ColorGrano, "amarillo naranja \\(F\\)")
  granoColor$`azul` <- stringr::str_detect(granoColor$ColorGrano, "azul \\(K\\)")
  granoColor$`azul oscuro` <- stringr::str_detect(granoColor$ColorGrano, "azul oscuro \\(L\\)")
  granoColor$`blanco` <- stringr::str_detect(granoColor$ColorGrano, "blanco \\(A\\)")
  granoColor$`blanco cremoso` <- stringr::str_detect(granoColor$ColorGrano, "blanco cremoso")
  granoColor$`blanco puro` <- stringr::str_detect(granoColor$ColorGrano, "blanco puro \\(H\\)")
  granoColor$`café` <- stringr::str_detect(granoColor$ColorGrano, "café")
  granoColor$`crema` <- stringr::str_detect(granoColor$ColorGrano, "crema")
  granoColor$`jaspeado` <- stringr::str_detect(granoColor$ColorGrano, "jaspeado")
  granoColor$`morado` <- stringr::str_detect(granoColor$ColorGrano, "morado")
  granoColor$`naranja` <- stringr::str_detect(granoColor$ColorGrano, "naranja")
  granoColor$`negro` <- stringr::str_detect(granoColor$ColorGrano, "negro")
  granoColor$`rojo` <- stringr::str_detect(granoColor$ColorGrano, "rojo \\(I")
  granoColor$`rojo naranja` <- stringr::str_detect(granoColor$ColorGrano, "rojo naranja \\(J")
  granoColor$`rojo oscuro` <- stringr::str_detect(granoColor$ColorGrano, "rojo oscuro")
  granoColor$`rosa` <- stringr::str_detect(granoColor$ColorGrano, "rosa")

  granoColor <- granoColor[,-4]

  granoColor <- granoColor |>
    dplyr::group_by(Localidad) |>
    dplyr::summarise(across(.cols = c(amarillo:rosa), .fns = sum)) |>
    tidyr::pivot_longer(cols = c(amarillo:rosa), names_to = "Color", values_to = "Valor") |>
    dplyr::mutate(Color = stringr::str_to_sentence(Color))

  granoColorT <- granoColor |>
    dplyr::group_by(Localidad) |>
    dplyr::summarise(Total = sum(Valor))

  granoColor <- granoColor |>
    dplyr::inner_join(granoColorT, by = "Localidad") |>
    dplyr::transmute(Localidad,
                     Color,
                     Valor = round(Valor / Total, 2))

  p1 <- granoColor |>
    dplyr::group_by(Localidad) |>
    echarts4r::e_chart(Color, timeline = TRUE) |>
    echarts4r::e_color(background = "#fffce2") |>
    echarts4r::e_pie(Valor,
                     percentPrecision = 0,
                     label = list(show = FALSE,
                                  position = "outside",
                                  formatter = '{b}: {d}%')) |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_choro_formatter("percent")) |>
    echarts4r::e_theme_custom('{"color":["#FBEC5D", "#FFF380", "#FFE66D", "#FFA542",
                              "#00BFFF", "#0000FF", "#FFFFFF", "#FAFAD2",
                              "#FFFAFA", "#A52A2A", "#FFF8DC", "#D1D1D1",
                              "#800080", "#FFA500", "#000000", "#FF0000",
                              "#FF4500", "#8B0000", "#FFC0CB"]}') |>
    echarts4r::e_toolbox_feature('saveAsImage') |>
    echarts4r::e_title("Color de los granos")

  granoCuanti <- dataOT[,-c(11:14)] |>
    tidyr::pivot_longer(cols = c(4:10), names_to = "Metrica", values_to = "Valor") |>
    dplyr::filter(!is.na(Valor)) |>
    dplyr::mutate(Metrica = factor(Metrica,
                                 levels = c("NumeroGranos", "PesoSemillas", "GranoLongitud",
                                            "GranoAnchura", "GranoGrosor", "Volumen100Granos",
                                            "Peso100Granos"),
                                 labels = c("Número de\ngranos", "Peso [g]",
                                            "Longitud", "Anchura",
                                            "Grosor", "Volumen 100\ngranos",
                                            "Peso 100\ngranos"),
                                 ordered = T))

  p2 <- granoCuanti |>
    dplyr::group_by(Metrica) |>
    echarts4r::e_chart(Localidad, timeline = TRUE) |>
    echarts4r::e_color(background = "#fffce2") |>
    echarts4r::e_scatter(Valor, colorBy = 'data',
                         symbol_size = 20,
                         legend = FALSE) |>
    echarts4r::e_toolbox_feature('saveAsImage') |>
    echarts4r::e_timeline_serie(title = list(
      list(text = "Número de granos"),
      list(text = "Peso de los granos [g]"),
      list(text = "Longitud de granos [cm]"),
      list(text = "Anchura de granos [cm]"),
      list(text = "Grosor de granos [cm]"),
      list(text = "Volumen de 100 granos [cm3]"),
      list(text = "Peso de 100 granos [g]")
    )) |>
    echarts4r::e_tooltip(trigger = "item")

  pf <- list(p1, p2)
  return(pf)
}
