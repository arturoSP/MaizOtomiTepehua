#' carousel
#'
#' @description Prepares a carousel of pictures to show on the app.
#'
#' @return a slickR object
#'
#' @noRd
#' @import slickR
#'

doCarousel <- function(){
  uri <- c("./inst/app/www/012paisaje.jpeg",
           "./inst/app/www/02deity.jpeg",
           "./inst/app/www/032manos.jpeg",
           "./inst/app/www/04deity.jpeg",
           "./inst/app/www/052maiz.jpeg",
           "./inst/app/www/06deity.jpeg",
           "./inst/app/www/072maiz.jpeg",
           "./inst/app/www/082maiz.jpeg",
           "./inst/app/www/092maiz.jpeg",
           "./inst/app/www/10manos.jpeg")
  pics <- data.frame(id = seq_len(length(uri)),
                     uri)

  p1 <- slickR::slickR(obj = pics$uri)+
    slickR::settings(dots = TRUE)

  return(p1)
}

