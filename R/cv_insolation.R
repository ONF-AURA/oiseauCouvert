#' Insolation reçue à la hauteur du seuil de la strate basse
#'
#' @param conf conf
#' @param mnh liste de mnh
#'
#' @return liste: rasters des 2 années + leaflet + table id + ggplot
#' @export
#'

cv_insolation <- function(dos_proj = data_conf("dos_projet"),
                          path_mnt = data_conf("path_mnt"),
                          path_mnh = data_conf("path_mnh_ts"),
                          shp = data_conf("shp"),
                          lim_h_rege = data_conf("lim_h_rege"),
                          dest_insol = data_conf("path_insolation_ts"),
                          resolution = 10,
                          force = FALSE){


  if(file.exists(dest_insol) & ! force){

    return(uRast("insolation"))
  }

  mnh <- uRast("mnh", origine = "-spot") %>%
    mask(dc("shp") %>% as("SpatVector"))
    # aggregate(resolution, na.rm = TRUE)

  mnt <- terra::rast(path_mnt)  %>%
    mask(dc("shp") %>% as("SpatVector"))
  # aggregate(resolution, na.rm = TRUE) %>% terra::crop(mnh[[1]])

  ls <- purrr::map(names(mnh), function(n){

    message("Insolation AN ", n, "...")

    mnhn <- mnh %>% terra::subset(n)

    insol <- cv.sun(util_spat2rast(mnt + mnhn))

    insol_rege <- terra::rast(insol) %>% terra::crop(mnhn)
    insol_rege[mnhn > lim_h_rege] <- NA
    # insol_rege.10 <- terra::aggregate(insol_rege, 10, na.rm = TRUE)
    # # NON !!! utiliser la somme avec NA=0 et non la moyenne
    #
    # # synthèse à l'id
    #
    # id_r <- terra::rasterize(shp %>% as("SpatVector"), insol_rege.10, field = "id")
    #
    #
    # tab_enso_id <- data.frame(
    #   id = terra::values(id_r) %>% as.numeric,
    #   inso = terra::values(insol_rege.10) %>% as.numeric,
    #   stringsAsFactors = FALSE) %>%
    #   na.omit %>%
    #   dplyr::group_by(id) %>%
    #   dplyr::summarise(insol.mean = mean(inso, na.rm = TRUE),
    #             insol.sd = sd(inso, na.rm = TRUE),
    #             area = n()) %>%
    #   dplyr::mutate(ymin = insol.mean - insol.sd,
    #          ymax = insol.mean + insol.sd,
    #          id = (terra::levels(id_r)[[1]])$id) %>%
    #   dplyr::arrange(insol.mean) %>%
    #   dplyr::left_join(shp %>%
    #                      dplyr::mutate(area_id = sf::st_area(.) %>% as.numeric) %>%
    #               as.data.frame %>% dplyr::select(id, area_id),
    #             by = "id") %>%
    #   dplyr::mutate(pc = area / area_id)
    #
    # scale <- max(tab_enso_id$ymax) / max(tab_enso_id$pc)
    #
    # plot_id <- ggplot2::ggplot(tab_enso_id %>% dplyr::mutate(ID = factor(id, levels = tab_enso_id$id)),
    #                            ggplot2::aes(x = ID, y = insol.mean, ymin = ymin, ymax = ymax))+
    #   ggplot2::geom_col(mapping = ggplot2::aes(y = pc * scale), fill = "#999999")+
    #   ggplot2::geom_boxplot()+
    #   ggplot2::geom_errorbar()+
    #   ggplot2::geom_text(ggplot2::aes(y = 5, label = ID), angle = 90, hjust = 0, color="black") +
    #   ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~./scale*100, name="surface ouverte (%)"))+
    #   ggplot2::theme(axis.text.x = ggplot2::element_blank(),
    #         axis.ticks.x = ggplot2::element_blank())
    #
    # return(list(
    #   insol_rege = insol_rege,
    #   tab_enso_id = tab_enso_id,
    #   plot_id = plot_id
    # ))

    list(insol_rege = insol_rege)
  })

  ts <- do.call(c, purrr::map(ls, ~.x$insol_rege))
  terra::time(ts) <- terra::time(mnh)
  names(ts) <- names(mnh)

  tsa <- ts %>% aggregate(resolution, fun = sum, na.rm = TRUE) / 1e8
  names(tsa) <- paste0("insol_", terra::time(mnh) %>% format("%Y"))
  terra::time(tsa) <- terra::time(mnh)

  data.writeCDF(tsa, dest_insol, "insolation")


  return(tsa)
}


