#' fct_mapa
#'
#' @description Prepares data and prints a map showing the origin of the collected
#' samples.
#'
#' @return An interactive leaflet map.
#'
#' @noRd
#' @import leaflet
#' @importFrom sf st_read
#'

plotMap <- function(){
  # lectura de datos para mapear
  dataOT <- bdMaizOT[,c(2, 3, 29, 30)]

  # le damos un poco de aleatorización a las coordenadas
  dataOT[,c(3)] <- dataOT[,c(3)] + runif(65)*0.01
  dataOT[,c(4)] <- dataOT[,c(4)] + runif(65)*0.01

  # lectura de mapa de municipios
  Mpios <- sf::st_read(dsn = "./data/MpiosOT.gpkg")

  getColor <- function(dataOT){
    dataOT$LocCol = dataOT$Municipio
      # dplyr::mutate(LocCol = Localidad)
    #          Level = ifelse(Level == "N", 6, Level),
    #          Level = ifelse(is.na(Level), 6, Level),
    #          Level = as.numeric(Level))
    sapply(dataOT$LocCol, function(LocCol) {
      if(LocCol == "San Bartolo Tutotepec") {
        "lightblue"
      } else if(LocCol == "Acaxochitlán") {
        "purple"
      } else if(LocCol == "Tenango de Doria") {
        "darkpurple"
      } else {
        "pink"
      }
    })
  }

  F_MapParam <- function(dataOT) {
    icons <- awesomeIcons(
      icon = "ios-information",
      iconColor = "black",
      library = "ion",
      markerColor = getColor(dataOT)
    )

    MpioLabel <- Mpios$NOMGEO

    map1 <- dataOT |>
      leaflet()  |>
      addProviderTiles(providers$Esri.NatGeoWorldMap) |>
      setView(lat = 20.28702, lng = -98.16721, zoom = 10) |>
      addPolygons(data = Mpios,
                  weight = 2,
                  opacity = 0.5,
                  fillOpacity = 0.1,
                  color = "black",
                  fillColor = "purple",
                  highlightOptions = highlightOptions(weight = 5,
                                                      color = "purple",
                                                      bringToFront = T),
                  label = MpioLabel,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "12px",
                                              direction = "auto")
                  ) |>
      addAwesomeMarkers(lng = dataOT$Longitud, lat = dataOT$Latitud,
                        icon = icons,
                        popup = dataOT$Localidad
      )

    return(map1)
  }

  p1 <- F_MapParam(dataOT)

  return(p1)
}

