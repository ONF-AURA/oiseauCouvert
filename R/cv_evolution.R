#' Processus complet d'estimation de l'évolution du couvert
#'
#' @param .dir chemin du dossier du projet
#' @param force_recalc TRUE pour recalculer
#'
#' @return rien: écriture des données sous .dir
#' @export
#'
cv_evolution <- function(.dir = get(".dir", pos = 1), force_recalc = FALSE){

  tryCatch(.dir, error = function(e){stop(
    crayon::red("ERREUR: "), "Aucun projet n'est chargé.\n",
    "Pour charger un projet:",
    crayon::italic(crayon::green("oiseauData::data_dir('/chemin/du/dossier/projet')"))
  )})

  dc <- oiseauData::data_conf

  # desserte

  if(is.null(dc("desserte"))){

    message("desserte: recherche des routes de la BD TOPO IGN...")

    dc("desserte", set = oiseauIGN::ign_get_desserte(dc("shp"), buffer = dc("buffer")))
  }

  # mns lidarHD

  if(!file.exists(dc("path_mns")) | force_recalc){

    message("mns: extraction du modèle numérique de surface du LiDARHD IGN...")

    oiseauIGN::ign_mns_lidarhd(dc("shp") %>% sf::st_buffer(dc("buffer")),
                               path_ta = dc("dir_ta_lidarhd"),
                               dos_laz = dc("dos_laz"),
                               dest = dc("dos_mns"))

    message("mnt: modèle numérique de terrain...")

    mnt <- oiseauData::data_mnt(dc("shp") %>% sf::st_buffer(dc("buffer")+1)) %>%
      terra::resample(terra::rast(dc("path_mns")))
    terra::writeRaster(mnt, dc("path_mnt"), overwrite = TRUE)

    message("mnh1: hauteurs du LiDARHD IGN...")

    mnh1 <- terra::rast(dc("path_mns")) - terra::rast(dc("path_mnt"))
    terra::writeRaster(mnh1, dc("path_mnh1"), overwrite = TRUE)

    message("mnh0: hauteurs initiales...")

    mnh0all <- terra::rast(dc("path_mnh"))
    mnh0 <- terra::resample(mnh0all, mnh1)
    terra::writeRaster(mnh0, dc("path_mnh0"), overwrite = TRUE)

    message("Evolution du couvert...")

    cv_carto_evo_frt(force_recalc = T)

    evo_futaie <- cv_color_evo(terra::rast(dc("path_evo_futaie")), plot = TRUE, shp = dc("shp"))
    evo_futaie_100 <- cv_color_evo(terra::rast(dc("path_evo_futaie100")))
    evo_renouv <- cv_color_evo(terra::rast(dc("path_evo_renouv")), plot = TRUE, shp = dc("shp"))
    evo_renouv_100 <- cv_color_evo(terra::rast(dc("path_evo_renouv100")))

    # Emprises EDF

    if(!file.exists(dc("path_emprise_edf"))){

      message("ligne électriques: recherche des lignes de la BD TOPO IGN...")

      msk = cv_mask_ligne_edf(dest = dc("path_emprise_edf"))
    }

    ref <- function(x){
      crayon::italic(crayon::green(x))
    }

    message(
      crayon::green("Evolution du couvert entre ", dc("an0"), " et ", dc("an1"), " calculée avec succès.\n"),
      "Les chemins des rasters sont disponibles en paramètres:\n",
      "📁 ", ref('path_evo_futaie'), " et ", ref('path_evo_futaie100'), " pour l'évolution des strates supérieures à la résolution respective de 1 et 10m\n",
      "📁 ", ref('path_evo_renouv'), " et ", ref('path_evo_renouv'), " pour l'évolution des strates inférieures à la résolution respective de 1 et 10m\n",
      "Visualisation: evo_futaie <- oiseauCouvert::cv_color_evo(terra::rast(dc('path_evo_futaie')), plot = TRUE, shp = dc('shp'))"

    )

  }
}
