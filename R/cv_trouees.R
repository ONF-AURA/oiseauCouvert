#' Identification des trouées à suivre
#'
#' @param conf conf
#' @param min_size surface mini de la trouée en ares
#'
#' @return conf
#' @export
#'

cv_trouees <- function(min_size = 3){


  types <- oiseauCouvert::cv_types_evo()

  renouv <- terra::rast(dc("path_evo_renouv"))

  trouees <- renouv
  trouees[is.na(trouees)] <- 0
  terra::values(trouees) <- (terra::values(renouv) %>% as.numeric() %in%
                               (types %>% dplyr::filter(strate1 == "rege") %>% dplyr::pull(cat)))
  trouees <- terra::mask(trouees, dc("shp") %>% as("SpatVector"))

  trouees10 <- terra::aggregate(trouees, 10, na.rm = TRUE)
  trouees10[trouees10 < .5] <- NA
  trouees10[!is.na(trouees10)] <- 1

  trouees_id_all <- terra::patches(trouees10)
  size <- terra::as.data.frame(trouees_id_all) %>%
    dplyr::mutate(id = last(.)) %>%
    dplyr::count(id) %>%
    dplyr::filter(n > min_size)


  trouees_id <- trouees_id_all
  trouees_id[!  terra::values(trouees_id) %in% size$id] <- NA
  trouees_id <- terra::patches(trouees_id)

  dc("path_trouees_suivies", set =file.path(dc("dos_projet"), "troueees_suivies.tif"))

  terra::writeRaster(trouees_id, dc("path_trouees_suivies"), overwrite = TRUE)

  # insolation des trouées

  ensol_raster <- rast(conf$path_insol_rege1_10)

  df <- as.data.frame(c(trouees_id,
                        ensol_raster))
  names(df) <- c("id", "enso")

  df <- df %>% mutate(id = as.character(id))

  dfs <- df %>%
    group_by(id) %>% summarise(surface = n()/100) %>%
    arrange(surface) %>%
    mutate(num = as.factor(1:nrow(.)))

  df <- df %>% left_join(
    dfs %>% dplyr::select(id, num)
  )
  scale <- quantile(df$enso, .99) / max(dfs$surface)

  ggplot()+
    geom_col(data = dfs,
             aes(y = surface * scale, x = num), fill = "gray") +
    geom_boxplot(data = df, aes(y = enso, x = num), outlier.alpha = 0) +
    scale_y_continuous(sec.axis = sec_axis(~./scale, name="surface (ha)")) +
    scale_x_discrete(labels = dfs$id)+
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

