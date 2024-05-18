#' Read data
#'
#' @importFrom stringr str_detect
#' @noRd
#'

# Datos de maíces ----
bdMaizOT <- read.csv("./data/bdMaiz.csv")#[,-1]
bdMaizOT <- type.convert(bdMaizOT, na.strings = "NA", as.is = FALSE)

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

# Datos de productores ----
bdProductoresOT <- read.csv("./data/bdProductores.csv")
colnames(bdProductoresOT) <- c("LenguaIndigena",   "Escolaridad",
                               "Familia",          "TiempoCompleto",
                               "PropiedadTerreno", "FechaPreparacion",
                               "FechaSiembra",     "FechaCosecha",
                               "Preparacion",      "Plagas",
                               "Milpa",            "Usos",
                               "Alimento",         "Problemas",
                               "SemillasHibridas")

