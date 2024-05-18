#' productores
#'
#' @description Prepares data for printing plot about the producers
#'
#' @return An interactive plot
#'
#' @noRd
#'

plotProductores <- function(){
  # características productores ----
  dbprod <- bdProductoresOT[,c("LenguaIndigena", "Escolaridad", "PropiedadTerreno")]
  nProd <- dim(dbprod)[1]

  dbprod <- dbprod |>
    tidyr::pivot_longer(cols = c(1:3), names_to = "propiedad", values_to = "valor")

  dbprod <- dbprod |>
    dplyr::group_by(propiedad,valor) |>
    dplyr::summarize(total = dplyr::n()) |>
    dplyr::mutate(porcentaje = total / nProd)

  p1 <- dbprod |>
    dplyr::group_by(propiedad) |>
    echarts4r::e_chart(valor, timeline = TRUE) |>
    echarts4r::e_color(background = "#FFA694") |>
    echarts4r::e_pie(porcentaje,
                     percentPrecision = 0,
                     label = list(show = FALSE,
                                  position = "outside",
                                  formatter = '{b}: {d}%')) |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_choro_formatter("percent")) |>
    echarts4r::e_toolbox_feature('saveAsImage') |>
    echarts4r::e_timeline_serie(title = list(
      list(text = "Escolaridad"),
      list(text = "Hablante de lengua indígena"),
      list(text = "Propiedad del terreno de siembra"))
    )

  # contexto producción y milpa ----
  dbContexto <- bdProductoresOT[,c("Preparacion", "Milpa")]

  tValor <- unlist(stringr::str_split(dbContexto$Preparacion, ";"))
  nValor <- length(tValor)
  tProp <- rep("Preparación", times = nValor)
  tContexto <- cbind(tProp, tValor)

  tValor <- unlist(stringr::str_split(dbContexto$Milpa, ";"))
  nValor <- length(tValor)
  tProp <- rep("ContextoMilpa", times = nValor)
  dbContexto <- as.data.frame(rbind(tContexto, cbind(tProp, tValor)))
  colnames(dbContexto) <- c("propiedad", "valor")

  dbContexto <- dbContexto |>
    dplyr::group_by(propiedad,valor) |>
    dplyr::summarize(total = dplyr::n()) |>
    dplyr::mutate(porcentaje = total / nProd)

  p2 <- dbContexto |>
    dplyr::filter(propiedad == "ContextoMilpa") |>
    dplyr::group_by(propiedad) |>
    echarts4r::e_chart(valor) |>
    echarts4r::e_color(background = "#FFA694") |>
    echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent"), max = 1) |>
    echarts4r::e_bar(serie = porcentaje,
                     label = list(show = FALSE,
                                  position = "outside",
                                  formatter = '{b}: {c}%'),
                     colorBy = 'valor',
                     showBackground = TRUE,
                     orient = "horizontal") |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_choro_formatter("percent")) |>
    echarts4r::e_toolbox_feature('saveAsImage') |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_title("Otros cultivos en la milpa")

  p3 <- dbContexto |>
    dplyr::filter(propiedad == "Preparación") |>
    dplyr::group_by(propiedad) |>
    echarts4r::e_chart(valor) |>
    echarts4r::e_color(background = "#FFA694") |>
    echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent"), max = 1) |>
    echarts4r::e_bar(serie = porcentaje,
                     label = list(show = FALSE,
                                  position = "outside",
                                  formatter = '{b}: {c}%'),
                     colorBy = 'valor',
                     showBackground = TRUE,
                     orient = "horizontal") |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_choro_formatter("percent")) |>
    echarts4r::e_toolbox_feature('saveAsImage') |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_title("Preparación del terreno")

  # usos de la planta y comida ----
  dbAlimento <- bdProductoresOT[,c("Usos", "Alimento")]

  tValor <- unlist(stringr::str_split(dbAlimento$Usos, ";"))
  nValor <- length(tValor)
  tProp <- rep("Usos", times = nValor)
  tAlimento <- cbind(tProp, tValor)

  tValor <- unlist(stringr::str_split(dbAlimento$Alimento, ";"))
  nValor <- length(tValor)
  tProp <- rep("Alimento", times = nValor)
  dbAlimento <- as.data.frame(rbind(tAlimento, cbind(tProp, tValor)))
  colnames(dbAlimento) <- c("propiedad", "valor")

  dbAlimento <- dbAlimento |>
    dplyr::group_by(propiedad,valor) |>
    dplyr::summarize(total = dplyr::n()) |>
    dplyr::mutate(porcentaje = total / nProd,
                  valor = stringr::str_wrap(valor, width = 15))

  p4 <- dbAlimento |>
    dplyr::filter(propiedad == "Alimento") |>
    dplyr::group_by(propiedad) |>
    echarts4r::e_chart(valor) |>
    echarts4r::e_color(background = "#FFA694") |>
    echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent"), max = 1) |>
    echarts4r::e_bar(serie = porcentaje,
                     label = list(show = FALSE,
                                  position = "outside",
                                  formatter = '{b}: {c}%'),
                     colorBy = 'valor',
                     showBackground = TRUE,
                     orient = "horizontal") |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_choro_formatter("percent")) |>
    echarts4r::e_toolbox_feature('saveAsImage') |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_title("Alimentos que preparan con maíz nativo")

  p5 <- dbAlimento |>
    dplyr::filter(propiedad == "Usos") |>
    dplyr::group_by(propiedad) |>
    echarts4r::e_chart(valor) |>
    echarts4r::e_color(background = "#FFA694") |>
    echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent"), max = 1) |>
    echarts4r::e_bar(serie = porcentaje,
                     label = list(show = FALSE,
                                  position = "outside",
                                  formatter = '{b}: {c}%'),
                     colorBy = 'valor',
                     showBackground = TRUE,
                     orient = "horizontal") |>
    echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_choro_formatter("percent")) |>
    echarts4r::e_toolbox_feature('saveAsImage') |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_title("Usos que se le da a la planta de maíz")

  # fechas importantes ----
  dbfechas <- bdProductoresOT[,c("FechaPreparacion", "FechaSiembra", "FechaCosecha")]
  tfechas <- data.frame(matrix(nrow = nProd * 2, ncol = 3))
  colnames(tfechas) <- c("Prep", "Siem", "Cose")

  for(i in seq_len(nProd)){
    t1 <- stringr::str_to_sentence(unlist(stringr::str_split(dbfechas[i,1], pattern = "-")))
    tfechas[i,1] <- t1[1]
    tfechas[(i+nProd),1] <- ifelse(is.na(t1[2]), t1[1], t1[2])
    #tfechas[i,2] <- t1[2]
    t1 <- stringr::str_to_sentence(unlist(stringr::str_split(dbfechas[i,2], pattern = "-")))
    tfechas[i,2] <- t1[1]
    tfechas[(i+nProd),2] <- ifelse(is.na(t1[2]), t1[1], t1[2])
    #tfechas[i,4] <- t1[2]
    t1 <- stringr::str_to_sentence(unlist(stringr::str_split(dbfechas[i,3], pattern = "-")))
    tfechas[i,3] <- t1[1]
    tfechas[(i+nProd),3] <- ifelse(is.na(t1[2]), t1[1], t1[2])
    #tfechas[i,6] <- t1[2]
  }

  tfechas <- tfechas |>
    dplyr::transmute(Preparacion = factor(Prep,
                                          levels = c("Enero", "Febrero", "Marzo",
                                                    "Abril", "Mayo", "Junio",
                                                    "Julio", "Agosto", "Septiembre",
                                                    "Octubre", "Noviembre", "Diciembre"),
                                          ordered = T),
                     Siembra = factor(Siem,
                                      levels = c("Enero", "Febrero", "Marzo",
                                                 "Abril", "Mayo", "Junio",
                                                 "Julio", "Agosto", "Septiembre",
                                                 "Octubre", "Noviembre", "Diciembre"),
                                      ordered = T),
                     Cosecha = factor(Cose,
                                      levels = c("Enero", "Febrero", "Marzo",
                                                 "Abril", "Mayo", "Junio",
                                                 "Julio", "Agosto", "Septiembre",
                                                 "Octubre", "Noviembre", "Diciembre"),
                                      ordered = T)
                     )
  p6 <- tfechas |>
    dplyr::arrange(Preparacion, Siembra, Cosecha) |>
    echarts4r::e_charts() |>
    echarts4r::e_color(color = c("blue", "green", "yellow"), background = "#FFA694") |>
    echarts4r::e_parallel(Preparacion, Siembra, Cosecha, opts = list(smooth = TRUE,
                                                                     colorBy = "Preparacion",
                                                                     lineStyle = list(width = 4))) |>
    echarts4r::e_toolbox_feature('saveAsImage')  |>
    echarts4r::e_title("Tiempos para el proceso del maíz") |>
    echarts4r::e_tooltip()

  # problemas ----
  dbProblemas <- bdProductoresOT[,c("Problemas", "SemillasHibridas")]
  dbProblemas[,2] <- ifelse(dbProblemas[,2] == "No", NA, "Semillas híbridas")
  tProb <- unlist(stringr::str_split(dbProblemas[,1], pattern = ";"))
  tProb <- c(tProb, dbProblemas[,2])

  p7 <- data.frame(tProb) |>
    dplyr::group_by(tProb) |>
    dplyr::reframe(Cuenta = dplyr::n()) |>
    dplyr::filter(!is.na(tProb)) |>
    echarts4r::e_charts() |>
    echarts4r::e_color(color = c("blue", "green", "yellow"), background = "#FFA694") |>
    echarts4r::e_cloud(word = tProb, freq = Cuenta, shape = "circle") |>
    echarts4r::e_toolbox_feature('saveAsImage')  |>
    echarts4r::e_title("Principales problemas que enfrentan los productores")


  pf <- list(p1, p2, p3, p4, p5, p6, p7)
  return(pf)
}
