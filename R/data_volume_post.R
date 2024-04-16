#' Table des volumes disparus après la date du dernier MNH
#'
#'
#'
#' @return table
#' @export
#'
data_volume_post <- function(shp = data_conf("shp"),
                          path_mnh = data_conf("path_mnh_ts"),
                          path_deads_ts = data_conf("path_deads_ts"),
                          path_emprise_edf.fac = data_conf("path_emprise_edf")
                          ){


  mnh <- terra::rast(path_mnh)

  # emprise EDF
  msk_edf <- tryCatch(terra::rast(path_emprise_edf.fac),
                      error = function(e){r <- mnh[[1]]; terra::values(r) <- NA; r})


  mnh_mask <- tryCatch(terra::rast(path_deads_ts),
                       error = function(e){r <- mnh; terra::values(r) <- NA; r})

  mnh <- mnh %>% terra::mask(msk_edf, inverse = TRUE)






  terra::crs(mnh) <- terra::crs(mnh_mask) <- "+init=epsg:2154"

  mnh <- mnh %>% terra::resample(mnh_mask)


  mnh_prel <- purrr::map(1:length(names(mnh_mask)),
                         ~ mnh[[.x]] %>% terra::mask(mnh_mask %>% terra::subset(.x) %>%
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

  data_id %>% dplyr::select(!dplyr::starts_with("an_"))

  data_conf("tab_volumes_ans", set = r, replace = TRUE)
}
