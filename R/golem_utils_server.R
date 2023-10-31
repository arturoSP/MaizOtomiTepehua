#' Read data
#'
#' @importFrom stringr str_detect
#' @noRd
#'

# preparación de base de datos ----

# lectura de datos desde google sheets
# {
#   bdMaizOT <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1XwkyOf2cVFqZ1vU4WZ4yeekDlY7PhosdGpeikGL8Ydg/edit?usp=sharing",
#                                         na = "", sheet = 1)
#   write.csv(bdMaizOT, file = "./data/bdMaiz.csv")
# }

bdMaizOT <- read.csv("./data/bdMaiz.csv")#[,-1]
bdMaizOT <- type.convert(bdMaizOT, na.strings = "NA", as.is = FALSE)
# str(bdMaizOT$TipoGrano)
# summary(bdMaizOT$TipoGrano)

# convierte en factores
bdMaizOT$FormaMazorca <- factor(bdMaizOT$FormaMazorca,
                                levels = c("cilíndrica", "cilíndrica,cónica-cilíndrica",
                                           "cónica-cilíndrica", "cónica"),
                                ordered = T)

bdMaizOT$DisposicionHileras <- factor(bdMaizOT$DisposicionHileras,
                                      levels = c("irregular", "en espiral,irregular",
                                                 "en espiral", "regular", "recta"),
                                      ordered = T)

bdMaizOT$TipoGrano <- factor(bdMaizOT$TipoGrano,
                             levels = c("harinoso (A)", "semi-harinoso", "dentado (C)",
                                        "semi-dentado (D)", "semi-cristalino (E),semi-dentado (D)",
                                        "semi-cristalino (E)", "cristalino (F)",
                                        "reventador (G),semi-cristalino (E)", "reventador (G)",
                                        "ceroso"),
                             ordered = T)

# nuevas columnas
bdMaizOT$FormaMazorcaNum <- as.numeric(bdMaizOT$FormaMazorca)

bdMaizOT$DisposicionHilerasNum <- as.numeric(bdMaizOT$DisposicionHileras)

bdMaizOT$TipoGranoNum <- as.numeric(bdMaizOT$TipoGrano)

bdMaizOT$multicolorGrano <- ifelse(stringr::str_detect(bdMaizOT$ColorGrano, ","),1,0)

bdMaizOT$multicolorOlote <- ifelse(stringr::str_detect(bdMaizOT$ColorOlote, ","),1,0)

# arreglo final

#bdMaizOT <- bdMaizOT[,c(1:18, 24:28, 19:23)]
bdMaizOT <- bdMaizOT[,c(1:18, 27:31, 19:23, 25,26)]


