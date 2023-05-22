#' Détection des apex
#'
#' @param h MNH spatraster
#' @param lim_h_rege hauteur minimale des houppiers
#' @param pente pente de la fct de surface minimale
#' @param intercept intercept
#'
#' @return spatial points
#' @export
#'
cv_apex <- function(h, lim_h_rege, pente = .07, intercept = 2){

  hs <- terra::focal(h, w = matrix(1,3,3), fun = median, na.rm = TRUE)

  f <- function(x) {x * pente + intercept}


  ttops <- lidR::locate_trees(hs,
                        lidR::lmf(f, hmin = lim_h_rege, shape = "circular")
  ) %>%
    sf::as_Spatial()

  raster::crs(ttops) <- raster::crs(oiseauSpot::spot_spat2rast(h))

  ttops
}
