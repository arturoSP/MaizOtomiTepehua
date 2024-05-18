#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinycssloaders
#' @import bslib
#' @importFrom echarts4r echarts4rOutput
#' @importFrom plotly plotlyOutput
#' @importFrom magrittr %>%
#' @importFrom slickR slickROutput
#'
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bootstrapPage(
      theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
      fluidRow(
        div(
          class = "container-fluid p-5",
          style = "background-color: #FFF380;",
          # título
          h1("Maíces nativos en la región Otomí-Tepehua",
             class = "header shadow-light",
             style = "#background-color: #800080;
             font-family: 'Ubuntu', sans-serif;"),
          h2("Hidalgo, México",
             class = "header shadow-light"),
        )
      ),
      fluidRow(
        div(
          class = "container p-5",
          style = "background-color: #91dba5;",
          # introducción
          h2("La región Otomí-Tepehua",
             class = "header shadow-light"),
          div(class = "row",
              div(class = "col-md-6",
                  span("La región Otomí-Tepehua, también conocida como \"Sierra de Tenango\",
          se encuentra en la Sierra Oriental del estado de Hidalgo. Esta región
          está compuesta de montañas con pendientes suaves, pequeños llanos
          intramontañosos, y extensas zonas boscosas que contienen numerosos
          manantiales, arroyos y saltos de agua. Se ubica en los municipios de
          Acaxochitlán, Agua Blanca de Iturbide, Huehuetla, San Bartolo Tutotepec,
          Tenango de Doria y una parte de Metepec."),
                  br(), br(),
                  span("La región Otomí-Tepehua se considera única debido a su rica biodiversidad
          y su atractivo cultural. Alberga varias comunidades indígenas, incluyendo
          los nahuas, tepehuas y otomíes, siendo estos últimos los que conforman
          la población más numerosa en la zona. El idioma principal es el otomí,
          aunque también se habla el tepehua en algunas áreas. Los Tepehua son un
          grupo etnolingüístico poco conocido debido a la falta de investigaciones
          sobre su cultura y lengua materna, los cuales se concentran principalmente
          en Huehuetla. Los nahuas, que hablan el idioma náhuatl, se hallan
          principalmente en el municipio de Acaxochitlán."),
                  br(), br(),
                  p("Otra riqueza importante de la región es que se sigue
                    cultivando el maíz nativo mediante el sistema de milpa, donde
                    se pueden encontrar una gran variedad de colores. Estos maíces
                    y otras plantas nativas de la región forman la base alimenticia
                    de los campesinos indígenas, quienes preservan su amor por la
                    naturaleza y honran la herencia cultural de sus antepasados.
                    En resumen, la región Otomí-Tepehua es una joya de México, que
                    contiene una gran riqueza natural y cultural que merece ser
                    apreciada y protegida.")
                  ),
              div(class = "col-md-6",
                  slickR::slickROutput("carousel", height = 'auto')
                  )
              )
        )
      ),
      fluidRow(
        div(
          class = "container p-5",
          style = "background-color: #ffce7f;",
          # gráficos
          h2("Conoce las características de los maíces que se recolectaron",
             class = "header dark shadow-dark"),
          tabsetPanel(
            tabPanel("Color del grano",
                     withSpinner(echarts4r::echarts4rOutput("grafGranos1", width = "90%"),
                                 type = 1)),
            tabPanel("Características del grano",
                     withSpinner(echarts4r::echarts4rOutput("grafGranos2", width = "90%"),
                                 type = 1)),
            tabPanel("Mazorcas",
                     withSpinner(echarts4r::echarts4rOutput("grafMazorcas", width = "90%"),
                                 type = 1)),
            tabPanel("Similitud en variedades por municipio",
                     div(class = "row",
                         div(class = "col-md-7",
                             withSpinner(plotly::plotlyOutput("grafMDS", width = "100%"),
                                     type = 1)),
                         div(class = "col-md-5",
                             p("El MDS no métrico opera bajo la premisa de que datos similares
                               entre sí se representan como puntos cercanos en un espacio de
                               menor dimensión. Su algoritmo de optimización se encarga de
                               encontrar la disposición de puntos que minimiza la distancia
                               entre datos considerados similares."),
                             br(),
                             p("En el gráfico proporcionado, los maíces de Tenango de Doria,
                               representados por rombos, están ubicados cercanos entre sí.
                               Esta proximidad sugiere que estos maíces comparten similitudes
                               y, por lo tanto, probablemente pertenecen a una única variedad.
                               Por otro lado, los maíces de Acaxochitlán, representados por
                               cuadrados, están más dispersos en el gráfico. Esta dispersión
                               indica una mayor variabilidad entre los datos, sugiriendo la
                               posible presencia de diferentes variedades de maíz en un mismo
                               municipio."),
                             br(),
                             p("El \"stress\" en este contexto se refiere a una medida de la
                               calidad de ajuste del gráfico MDS no métrico. Indica qué tan
                               bien se preservan las relaciones de similitud entre los datos
                               en el espacio de menor dimensión en comparación con el espacio
                               original. Un valor de stress por debajo de 0.20
                               sugiere un buen ajuste, es decir, que la representación en el espacio
                               de menor dimensión captura de manera fiel las relaciones de
                               similitud entre los datos originales."))
                         )
                     )
            )
        )
      ),
      fluidRow(
        div(
          class = "container p-5",
          style = "background-color: #756bb1;",
          # mapa
          h2("Ubicación de origen de los maíces recolectados",
             class = "header dark shadow-dark"),
          withSpinner(leaflet::leafletOutput("grafMapa", width = "90%"),
                      type = 1)
        )
      ),
      fluidRow(
        div(
          class = "container p-5",
          style = "background-color: #FFFAC8;",
          # introducción
          h2("Los productores de maíz en la región Otomí-Tepehua",
             class = "header shadow-light"),
          div(class = "row",
              div(class = "col-md-6",
                  span("Los campesinos de la región Otomí-Tepehua mantienen viva
                       una tradición milenaria al cultivar maíz nativo.
                       Sorprendentemente, estos agricultores logran cosechar sus
                       maíces sin necesidad de riego, gracias a su eficiente
                       cultivo tradicional “sistema milpa”, que combina el maíz
                       con otros cultivos y árboles, permitiéndoles aprovechar
                       al máximo los recursos naturales disponibles."),
                  br(), br(),
                  span("El maíz nativo desempeña un papel fundamental en la
                  dieta diaria de los campesinos de la región Otomí-Tepehua,
                  siendo consumido principalmente en forma de tortilla. Aunque
                  suelen vender los excedentes de la producción, la mayor parte
                  del maíz se destina al autoconsumo, siendo un esfuerzo
                  colectivo de toda la familia.")
              ),
              div(class = "col-md-6",
                  span("Estos campesinos cultivan maíces nativos como una
                       herencia invaluable de sus padres y abuelos, demostrando
                       un compromiso arraigado con la biodiversidad. Es
                       fundamental resaltar que no utilizan plaguicidas
                       sintéticos en su proceso de producción, preservando así
                       la pureza de sus cultivos."),
                  br(), br(),
                  span("El trabajo conjunto de los campesinos de maíz nativo de
                       la región Otomí-Tepehua se revela como esencial para la
                       preservación tanto de la riqueza cultural como de la
                       biodiversidad en México.")
                  )
          )
        )
      ),
      fluidRow(
        div(class = "container p-5",
            style = "background-color: #F08080;",
            h2("Conoce a los productores",
               class = "header dark shadow-dark"),
            tabsetPanel(
              tabPanel("Productores",
                       withSpinner(echarts4r::echarts4rOutput("grafProductores", width = "90%"),
                                   type = 1)),
              tabPanel("Fechas importantes",
                       withSpinner(echarts4r::echarts4rOutput("grafFechas", width = "90%"),
                                   type = 1)),
              tabPanel("Otros cultivos",
                       withSpinner(echarts4r::echarts4rOutput("grafMilpa", width = "90%"),
                                   type = 1)),
              tabPanel("Preparación del terreno",
                       withSpinner(echarts4r::echarts4rOutput("grafPreparacion", width = "90%"),
                                   type = 1)),
              tabPanel("Usos",
                       withSpinner(echarts4r::echarts4rOutput("grafUsos", width = "90%"),
                                   type = 1)),
              tabPanel("Alimentos",
                       withSpinner(echarts4r::echarts4rOutput("grafAlimentos", width = "90%"),
                                   type = 1)),
              tabPanel("Problemas",
                       withSpinner(echarts4r::echarts4rOutput("grafProblemas", width = "90%"),
                                   type = 1))
            )
        )
      ),
      fluidRow(
        div(
          class = "container p-5",
          style = "background-color: #E0E0E0;",
          #ending
          p("Este proyecto fue desarrollado como parte de las actividades de la",
            tags$a(href="https://conahcyt.mx/",
                   "estancia posdoctoral CONAHCYT",
                   target="_blank"),
            "desarrollada por Dra. Aline Romero-Natale en la ",
            tags$a(href="https://www.uaeh.edu.mx/",
                   "UAEH",
                   target = "_blank"),
            ". Diseño y programación por ",
            tags$a(href="https://github.com/arturoSP",
                   "Dr. Arturo Sanchez-Porras",
                   target = "_blank"),
            ".")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", system.file('app/www', package = 'MaizOtomiTepehua')
  )

  tags$head(
    golem::favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MaizOtomiTepehua"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
