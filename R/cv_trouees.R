#' Identification des trouées à suivre
#'
#' @param min_size surface mini de la trouée en ares
#' @param txOuv taux d'ouverture au-dessus duquel le peuplement est concidéré comme trouée
#'
#' @return raster écrit sous "dest_trouees_suivies"
#' @export
#'

cv_trouees <- function(min_size = data_conf("min_size_trouees"),
                       txOuv = data_conf("tx_ouv_trouees"),
                       path_evo_renouv = data_conf("path_evo_renouv"),
                       path_deads_ts= data_conf("path_deads_ts"),
                       dest_trouees_suivies = data_conf("path_trouees_suivies"),
                       an0 = data_conf("an0")
                       ){



  # data_check("min_size_trouees","tx_ouv_trouees","path_evo_renouv","path_deads_ts","shp","dest_trouees_suivies")

  # raster zones en strate régé avec année

  tr = terra::rast(path_evo_renouv)
  ans = terra::rast(path_deads_ts)
  terra::crs(tr) <- terra::crs(ans) <- "epsg:2154"
  evo <- tr %>% terra::resample(ans)
  data <- c(evo, ans)
  names(data)[1] <- "evo"

  types <- data_typo_evo()

  types_ouv <- types %>% dplyr::filter(strate1 == "rege" & strate0 != "rege")

  ouv <- evo
  ouv[!ouv %in% types_ouv$cat] <- NA
  ouv[ouv %in% types_ouv$cat] <- an0

  for(a in names(data)[-1]){
    oa <- data[[a]]
    an <- stringr::str_remove_all(a, "an_") %>% as.numeric()
    ouv[oa == 0] <- an
  }

  names(ouv) <- "an"
  terra::plot(ouv)
  terra::plot(dc("shp") %>% as("SpatVector"), add=T)

  # trouées = zones suffisamment ouvertes

  trouees <- ouv
  trouees[!is.na(trouees)] <- 1
  trouees[is.na(trouees)] <- 0
  trouees <- terra::mask(trouees, dc("shp") %>% as("SpatVector"))

  trouees10 <- terra::focal(trouees, 9, fun = "mean")
  trouees10[trouees10 < txCv] <- NA
  trouees10[!is.na(trouees10)] <- 1

  # filter taille mini

  trouees_id_all <- terra::patches(trouees10)
  size <- terra::as.data.frame(trouees_id_all) %>%
    dplyr::mutate(id = last(.)) %>%
    dplyr::count(id) %>%
    dplyr::filter(n > min_size)


  trouees_id <- trouees_id_all
  terra::values(trouees_id)[,1][! terra::values(trouees_id)[,1] %in% size$id] <- NA
  # trouees_id <- terra::patches(trouees_id)

  trouees_size <- terra::classify(trouees_id, size)

  # année ouverture

  trouees_dates <- terra::mask(ouv, trouees_id)

  trouees_suivies <- c(trouees_id, trouees_size, trouee)

  terra::writeRaster(trouees_id, dest_trouees_suivies, overwrite = TRUE)

  # insolation des trouées

  oiseauLidar::cv_insolation()

  ensol_raster <- terra::rast(conf$path_insol_rege1_10)

  df <- as.data.frame(c(trouees_id,
                        ensol_raster))
  names(df) <- c("id", "enso")

  df <- df %>% dplyr::mutate(id = as.character(id))

  dfs <- df %>%
    dplyr::group_by(id) %>% dplyr::summarise(surface = n()/100) %>%
    dplyr::arrange(surface) %>%
    dplyr::mutate(num = as.factor(1:nrow(.)))

  df <- df %>% dplyr::left_join(
    dfs %>% dplyr::select(id, num)
  )
  scale <- quantile(df$enso, .99) / max(dfs$surface)

  ggplot2::ggplot()+
    ggplot2::geom_col(data = dfs,
                      ggplot2::aes(y = surface * scale, x = num), fill = "gray") +
    ggplot2::geom_boxplot(data = df, ggplot2::aes(y = enso, x = num), outlier.alpha = 0) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~./scale, name="surface (ha)")) +
    ggplot2::scale_x_discrete(labels = dfs$id)+
    ggplot2::ylab("Insolation")

  # carte insol trouées

  # terra::crs(ensol_raster) <- terra::crs(trouees_suivies)
  # raster_insol_trouees <- ensol_raster * trouees_suivies
  #
  # plot(raster_insol_trouees)
  #
  # conf$insol <- list(plaot_ug = plot)

  # saveRDS(conf, file.path(conf$dos_proj, "conf.rds"))

  # return(conf)
}

