#' Fonction V = f(mnh)
#'
#' @param fh FH
#' @param an année MNH utilisée
#' @param path_dendro chemin rasters dendrométriques
#' @param path_mnh chemin raster mnh (série temporelle)
#'
#' @return fonction
#' @export
#'
cv.fct_volume <- function(
    an,
    path_dendro = oiseauData::data_conf("path_dendro"),
    path_mnh = oiseauData::data_conf("path_mnh_ts"),
    fh = 10){

  # oiseauData::data_check("path_dendro", "path_mnh0")

  st <- dc("path_dendro") %>% terra::rast()
  mnh0 <- terra::rast(path_mnh) %>% terra::subset(as.character(an))

  terra::crs(st) <- terra::crs(mnh0) <- "+init=epsg:2154"
  cv26 <- terra::aggregate(mnh0, 26, fun = function(x) sd(x)) %>%
    terra::resample(st$g)

  mnh26 <- terra::resample(mnh0, st$g)
  names(mnh26) <- "h"
  cv26 <- terra::resample(mnh0^2, st$g)
  names(cv26) <- "h2"

  cv263 <- terra::resample(mnh0^3, st$g)
  names(cv263) <- "h3"

  df <- c(mnh26, cv26, cv263, g = st$g) %>% terra::as.data.frame()
  head(df)

  glm(g~h, data = df)
  glm(g~h+h2, data = df)
  glm(g~h+h2+h3, data = df)

  lm <- glm(g~h, data = df)

  function(h){
    v <- fh * (lm$coefficients[1] + lm$coefficients[2] * h)
    ifelse(v<0, 0, v)
  }
}
