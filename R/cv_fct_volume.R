#' Fonction V = f(mnh)
#'
#' @param fh FH
#'
#' @return fonction
#' @export
#'
cv_fct_volume <- function(fh = 10){

  st <- dc("path_st") %>% terra::rast()
  mnh0 <- terra::rast(dc("path_mnh0"))

  terra::crs(st) <- terra::crs(mnh0) <- "+init=epsg:2154"
  cv26 <- terra::aggregate(mnh0, 26, fun = function(x) sd(x)) %>%
    terra::resample(st$g)

  mnh26 <- terra::resample(mnh0, st$g)
  cv26 <- terra::resample(mnh0^2, st$g)
  cv263 <- terra::resample(mnh0^3, st$g)
  df <- c(mnh26, cv26, cv263, st$g) %>% terra::as.data.frame()
  head(df)

  glm(g~MNH, data = df)
  glm(g~MNH+MNH.1, data = df)
  glm(g~MNH+MNH.1+MNH.2, data = df)

  lm <- glm(g~MNH, data = df)

  function(h){
    v <- fh * (lm$coefficients[1] + lm$coefficients[2] * h)
    ifelse(v<0, 0, v)
  }
}
