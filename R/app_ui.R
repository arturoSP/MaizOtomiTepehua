#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinycssloaders
#' @import bslib
#' @import echarts4r
#' @importFrom magrittr %>%
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
             font-family: 'Ubuntu', sans-serif;")
        )
      ),
      fluidRow(
        div(
          class = "container p-5",
          style = "background-color: #91dba5;",
          # introducción
          h2("La región Otomí-Tepehua",
             class = "header shadow-light"),
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
          div(class = "row",
              div(class = "col-md-3",
                  img(src = "manos.png", align = "center")),
              div(class = "col-md-9",
                  br(),
                  p("Otra riqueza importante de la región es que se sigue
                    cultivando el maíz nativo mediante el sistema de milpa, donde
                    se pueden encontrar una gran variedad de colores. Estos maíces
                    y otras plantas nativas de la región forman la base alimenticia
                    de los campesinos indígenas, quienes preservan su amor por la
                    naturaleza y honran la herencia cultural de sus antepasados.
                    En resumen, la región Otomí-Tepehua es una joya de México, que
                    contiene una gran riqueza natural y cultural que merece ser
                    apreciada y protegida."))
              )
        )
      ),
      fluidRow(
        div(
          class = "container p-5",
          style = "background-color: #ffce7f;",
          # gráficos
          h2("Conoce los maíces que recolectamos",
             class = "header dark shadow-dark"),
          tabsetPanel(
            tabPanel("Color del grano",
                     withSpinner(echarts4r::echarts4rOutput("grafGranos1"),
                                 type = 1)),
            tabPanel("Características del grano",
                     withSpinner(echarts4r::echarts4rOutput("grafGranos2"),
                                 type = 1)),
            tabPanel("Mazorcas",
                     withSpinner(echarts4r::echarts4rOutput("grafMazorcas"),
                                 type = 1)),
            tabPanel("Similitud en variedades por localidad",
                     withSpinner(plotly::plotlyOutput("grafMDS"),
                                 type = 1))
            )
        )
      ),
      fluidRow(
        div(
          class = "container p-5",
          style = "background-color: #FFFAC8;",
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
