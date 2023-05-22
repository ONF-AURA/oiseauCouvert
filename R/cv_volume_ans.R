#' Table des volumes disparus après la date du dernier MNH
#'
#'
#'
#' @return table écrite dans conf$tab_volumes_ans
#' @export
#'
cv_volume_ans <- function(){

  # emprise EDF
  msk_edf <- terra::rast(dc("path_emprise_edf"))

  mnh1 <- terra::rast(dc("path_mnh1")) %>% terra::mask(msk_edf, inverse = TRUE)
  names(mnh1) <- "h1"

  mnh_mask <- terra::rast(dc("path_mnh_mask_ts"))

  terra::crs(mnh1) <- terra::crs(mnh_mask) <- "+init=epsg:2154"

  mnh1 <- mnh1 %>% terra::resample(mnh_mask)


  mnh_prel <- purrr::map(1:length(names(mnh_mask)),
                         ~ mnh1 %>% terra::mask(mnh_mask %>% terra::subset(.x) %>%
                                                  terra::classify(cbind(1, NA)))
  )
  mnh_prel[[1]] %>% terra::plot()
  names(mnh_prel) <- names(mnh_mask)

  mnh_prel$id <- dc("shp") %>% as("SpatVector") %>% terra::rasterize(mnh1, "id")

  data <- terra::rast(mnh_prel) %>% terra::as.data.frame(na.rm = FALSE) %>%
    dplyr::mutate_if(is.numeric, cv_fct_volume()) %>%
    dplyr::mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    dplyr::group_by(id)

  data_vol <- data %>%
    dplyr::summarise_all(mean, na.rm = TRUE)

  data_surf <- data %>% dplyr::summarise(surf = n() / 10000)

  data_id <- dplyr::left_join(data_vol, data_surf) %>%
    dplyr::filter(!is.na(id))

  for(col in names(mnh_mask)){
    data_id[[stringr::str_replace_all(col, "an_", "volha_")]] <- data_id[[col]]
    data_id[[stringr::str_replace_all(col, "an_", "vol_")]] <- data_id[[col]] * data_id$surf
  }

  r <- data_id %>% dplyr::select(!dplyr::starts_with("an_"))

  dc("tab_volumes_ans", set = r)
}
