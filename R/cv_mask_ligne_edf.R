# emprise EDF


#' Masque des emprises EDF
#'
#' @param dest fichier d'écriture du raster
#'
#' @return spatraster 1 = emprise, NA ailleurs
#' @export
#'
cv_mask_ligne_edf <- function(dest){

  mnh <- terra::rast(dc("path_mnh0")) %>% terra::classify(cbind(NA, 0))

  edf <- oiseauIGN::ign_wfs_request("topographie", "BDTOPO_V3", "ligne_electrique", epsg = 2154, bbox = dc("shp"))

  ls_msk <- purrr::map(1:nrow(edf), function(n){
    message("estimation de l'emprise ligne ", edf %>% dplyr::slice(n) %>% dplyr::pull(voltage),
            " - ", n, "/", nrow(edf))

    h_emp <- purrr::map_dbl(1:40, function(d){
      msk_edf <- edf %>% dplyr::slice(n) %>% sf::st_buffer(d) %>% as("SpatVector") %>% terra::rasterize(mnh)
      mnh %>% terra::mask(msk_edf) %>%
        terra::focal(fun = min) %>%
        terra::values() %>% median(na.rm = TRUE)
    })
    # plot(h_emp)
    buff_emp <- min(which(h_emp > 0))

    edf %>% sf::st_buffer(buff_emp) %>% as("SpatVector") %>% terra::rasterize(mnh)
  })

  msk <- do.call(c, ls_msk)
  r <- terra::mean(msk, na.rm = TRUE)

  terra::writeRaster(r, dest, overwrite = TRUE)

  r
}
