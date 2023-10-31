#' fct_mapa
#'
#' @description Prepares data and prints a map showing the origin of the collected
#' samples.
#'
#' @return An interactive leaflet map.
#'
#' @noRd
#' @import leaflet
#' @importFrom rgdal readOGR
#'

plotMap <- function(){
  # lectura de datos para mapear
  dataOT <- bdMaizOT[,c(2, 3, 29, 30)]

  # le damos un poco de aleatorización a las coordenadas
  dataOT[,c(3)] <- dataOT[,c(3)] + runif(65)*0.01
  dataOT[,c(4)] <- dataOT[,c(4)] + runif(65)*0.01

  # lectura de mapa de municipios
  Mpios <- rgdal::readOGR(dsn = "./data/MpiosOT.gpkg", stringsAsFactors = F)

  getColor <- function(dataOT){
    dataOT$LocCol = dataOT$Localidad
      # dplyr::mutate(LocCol = Localidad)
    #          Level = ifelse(Level == "N", 6, Level),
    #          Level = ifelse(is.na(Level), 6, Level),
    #          Level = as.numeric(Level))
    sapply(dataOT$LocCol, function(LocCol) {
      if(LocCol == "Agua Fría") {
        "lightblue"
      } else if(LocCol == "Agua Grande") {
        "purple"
      } else if(LocCol == "Cerro de la Luna") {
        "darkpurple"
      } else if(LocCol == "Cerro Verde") {
        "pink"
      } else if(LocCol == "La Cruz de Tenango") {
        "cadetblue"
      } else if(LocCol == "La Joya"){
        "white"
      } else if(LocCol == "Montemar"){
        "gray"
      } else if(LocCol == "Paredones"){
        "red"
      } else if(LocCol == "San Nicolás"){
        "orange"
      } else if(LocCol == "Santa María Temaxcalapa"){
        "beige"
      } else if(LocCol == "Techachalco"){
        "green"
      } else{
        "blue"
      }
    })
  }

  F_MapParam <- function() {
    icons <- awesomeIcons(
      icon = "ios-information",
      iconColor = "black",
      library = "ion",
      markerColor = getColor(dataOT)
    )

    map1 <- leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lat = 20.28702, lng = -98.16721, zoom = 10) %>%
      addPolygons(data = Mpios,
                  weight = 2,
                  opacity = 0.5,
                  fillOpacity = 0.1,
                  color = "black",
                  fillColor = "yellow"
      ) %>%
      addMarkers(lng = dataOT$Longitud, lat = dataOT$Latitud,
                 icon = icons,
                 popup = dataOT$Localidad
                 #clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = T),
                 #clusterId = "transectCluster")
      )

    return(map1)
  }

  p1 <- F_MapParam()

  return(p1)
}

