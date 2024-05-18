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
    stat_ellipse(aes(color = Grupo), fill = "#fffce2", type = "t", geom = "polygon", alpha = 0.3, show.legend = F)+
    geom_point(aes(shape = Municipio, fill = Municipio, color = Grupo), size = 4)+
    scale_shape_manual(values = c(15, 16, 17, 18))+
    scale_fill_manual(values = c("#1f78b4", "#fb8072", "#7fbc41",
                                 "#aaffb3"))+
    scale_color_manual(values = c("#b2182b", "#4393c3", "#de77ae"))+
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

  return(p1)

}
