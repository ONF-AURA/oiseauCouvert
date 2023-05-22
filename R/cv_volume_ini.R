#' Table des volumes à la date du dernier MNH
#'
#' volumes aux dates des MNH disponible, prélèvements et accroissement
#'
#' @param leaflet TRUE pour afficher la carte des résultats
#'
#' @return table écrite dans conf$tab_volumes_ini
#' @export
#'
cv_volume_ini <- function(leaflet = FALSE){

  # emprise EDF
  msk_edf <- terra::rast(dc("path_emprise_edf"))

  mnh0 <- terra::rast(dc("path_mnh0")) %>% terra::mask(msk_edf, inverse = TRUE)
  mnh1 <- terra::rast(dc("path_mnh1")) %>% terra::mask(msk_edf, inverse = TRUE)
  names(mnh0) <- "h0"
  names(mnh1) <- "h1"

  mnh0_d <- mnh0 %>% terra::clamp(8,50, values = FALSE)
  mnh1_d <- mnh1 %>% terra::clamp(0,50, values = TRUE)


  diff <- mnh1_d - mnh0_d
  r_diff <- diff %>% oiseauSpot::spot_spat2rast()

  disp <- diff %>% terra::clamp(upper = -1, values = FALSE)

  pa <- terra::patches(disp)
  pav <- terra::values(pa)[,1]
  t <- table(pav) %>% as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      pav = as.numeric(pav),
      new = ifelse(Freq < 4, NA, 1)) %>%
    dplyr::select(-Freq)

  msk <- terra::classify(pa, t)

  disp <- disp *  msk
  names(disp) <- "disp"

  # volumes par parcelle


  id <- dc("shp") %>% as("SpatVector") %>% terra::rasterize(disp, "id")


  fun_vol <- cv_fct_volume()

  tab0 <- c(
    mnh0 %>% terra::clamp(lower = 8, values = FALSE),
    mnh1 %>% terra::clamp(lower = 8, values = FALSE)
  ) %>% terra::as.data.frame() %>% dplyr::mutate(d=h1-h0)

  tab <- c(id,
           disp %>% terra::classify(cbind(NA, 0)),
           mnh0 %>% terra::classify(cbind(NA, 0)),
           mnh1 %>% terra::classify(cbind(NA, 0))
  ) %>% terra::as.data.frame() %>%
    dplyr::mutate(
      vrec = fun_vol(-disp),
      v0 = fun_vol(h0),
      v1 = fun_vol(h1)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(surf = n() / 10000,
                     vrec.ha = mean(vrec),
                     v0 = mean(v0),
                     v1 = mean(v1),
                     Vrec = vrec.ha * surf) %>%
    dplyr::mutate(vacc = (v1 - v0 + vrec.ha) / (dc("an1") - dc("an0")))

  dc("tab_volumes_ini", set = tab)

  if(leaflet){

    pal <- leaflet::colorNumeric("Greys", domain = 8:50)

    leaflet::leaflet() %>%
      oiseauIGN::ign_get_wms(short = "ortho", group = 'ortho') %>%
      leaflet::addRasterImage(disp %>% oiseauSpot::spot_spat2rast(), group = 'disp') %>%
      leaflet::addRasterImage(mnh0_d %>% oiseauSpot::spot_spat2rast(), colors = pal, group = 'mnh0_d') %>%
      leaflet::addRasterImage(mnh1_d %>% oiseauSpot::spot_spat2rast(), colors = pal, group = 'mnh1_d') %>%
      leaflet::addLayersControl(baseGroups = c("ortho", "mnh0_d", "mnh1_d"),
                                overlayGroups = "disp",
                                options = leaflet::layersControlOptions(collapsed = FALSE))
  }
}
