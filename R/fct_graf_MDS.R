#' graf_MDS
#'
#' @description Prepares data to show an interactive non-metric MDS plot for the
#' different collected samples.
#'
#' @return An interactive plot.
#'
#' @noRd
#' @importFrom vegan vegdist
#' @importFrom vegan metaMDS
#' @importFrom dplyr bind_cols
#' @importFrom stats kmeans
#' @importFrom stats princomp
#' @importFrom plotly ggplotly
#' @import ggplot2
#'

plotMDS <- function(){
  # transformación de los datos antes de matriz de distancia
  dataOT <- (bdMaizOT[,c(4:23)])^0.25

  # bdMaizOT[,1] %>%
  #   dplyr::bind_cols(dataOT) %>%
  #   tidyr::pivot_longer(cols = c(2:21),
  #                names_to = "Variable", values_to = "Valor") %>%
  #   ggplot(aes(x = Variable, y = Codigo, fill = Valor))+
  #   geom_tile()+
  #   theme(axis.text.x = element_text(angle = 90))


  # análisis de variables cuantitativas ----
  brayOT <- vegan::vegdist(dataOT, method = "bray", na.rm = T)

  MDS_OT <- vegan::metaMDS(brayOT, try = 50)
  MDS_OT_points <- MDS_OT$points

  claves <- bdMaizOT[,1:3]

  MDS_OT_points <- dplyr::bind_cols(MDS_OT_points,claves)
  MDS_OT_stress <- round(MDS_OT$stress,2)

  # determinación de clusters
  clust <- stats::kmeans(MDS_OT_points[,1:2], centers = 3)
  pca_MDS <- stats::princomp(MDS_OT_points[,1:2])
  MDS_cluster <- data.frame(pca_MDS$scores,
                            Grupo = as.factor(clust$cluster),
                            Localidad = as.factor(MDS_OT_points$Localidad),
                            Municipio = as.factor(MDS_OT_points$Municipio))

  p1 <- ggplot(MDS_cluster, aes(x = Comp.1, y = Comp.2))+
    stat_ellipse(aes(fill = Grupo), type = "t", geom = "polygon", alpha = 0.3, show.legend = F)+
    geom_point(aes(shape = Municipio, fill = Localidad, color = Grupo), size = 4)+
    scale_shape_manual(values = c(21, 22, 23, 24, 0, 1, 2))+
    scale_fill_manual(values = c("#b2182b", "#4393c3", "#7fbc41", "#de77ae",
                                 "#8dd3c7", "#aaffb3", "#bebada",
                                 "#fb8072", "#80b1d3", "#fdb462",
                                 "#a6cee3", "#1f78b4", "#b2df8a",
                                 "#33a02c", "#fb9a99", "#e31a1c",
                                 "#fdbf6f", "#ff7f00", "#cab2d6",
                                 "#6a3d9a", "#ffff99", "#b15928"))+
    scale_color_manual(values = c("#b2182b", "#4393c3", "#7fbc41", "#de77ae"))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.background = element_rect(fill = "#fffce2"),
          panel.background = element_rect(fill = "#fffce2"),
          legend.background = element_rect(fill = "#fffce2"),
          legend.key = element_rect(fill = "#fffce2"))+
    ggtitle(paste0("MDS no métrico, stress = ", MDS_OT_stress))

  p1 <- plotly::ggplotly(p1, tooltip = c("fill"))

  # # gráfico de análisis
  # p1 <- MDS_cluster |>
  #   #dplyr::mutate(Cluster = clust) |>
  #   dplyr::group_by(Localidad) |>
  #   echarts4r::e_chart(MDS1) |>
  #   echarts4r::e_color(background = "#fffce2") |>
  #   echarts4r::e_scatter(MDS2,
  #                        symbol_size = 10,
  #                        symbol = "circle",
  #                        rm_x = TRUE,
  #                        rm_y = TRUE) |>
  #   echarts4r::e_hide_grid_lines() |>
  #   echarts4r::e_circle_g(right = 0, top = 0, radius = 0.02) |>
  #   echarts4r::e_rm_axis(axis = c("MDS1", "MDS2")) |>
  #   echarts4r::e_toolbox_feature("saveAsImage") |>
  #   echarts4r::e_tooltip(trigger = "item") |>
  #   echarts4r::e_legend(orient = "vertical", right = '5', top = '10%') |>
  #   echarts4r::e_title("Similitud entre muestras recolectadas", paste0("MDS no métrico, stress = ", MDS_OT_stress))
  # p1

  return(p1)

}
