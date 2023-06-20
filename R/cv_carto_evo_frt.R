#' Cartographie des différences entre 2 mnh de dates différntes sur une forêt entière
#'
#' @param shp sf de la forêt, avec champ 'CCOD_PRF
#' @param mnh0 mnh an 0
#' @param mnh1 mnh an 1
#' @param an0 année premier mnh
#' @param an1 année second mnh
#' @param lim_h_rege seuil hauteur catégorie régénération
#' @param lim_h_perche id pour les jeunes peuplemments
#' @param lim_dh_rege seuil acc mini régé
#' @param lim_dh_perche seuil acc annuel fort/faible pour les ppts jeunes
#' @param lim_dh_futaie id pour les ppts adultes
#' @param max_acc accroissement max en m/an au-delà duquel une fermeture latérale du couvert est vraisemblable
#' @param max_area_perche surf max d'un houppier de h < lim_h_perche pour qu'il soit considéré comme encore jeune
#' @param seuil_dtpi différence de TPI au-delà duquel le pixel est considéré comme en fermeture latérale du couvert
#' @param champ_id champ d'identifiant unique de shp,
#' @param buffer tampon de traitement en m autour de shp,
#' @param path_desserte chemin du shapefile des dessertes avec champ 'largeur'
#' @param w largueur de la fenêtre d'aggrégation (impaire)
#' @param dir chemin du dossier du projet. si non NULL, utilise oiseauData::data_conf pour les arguments précédents
#' @param force_recalc TRUE pour calculer l'évolution même si la donnée existe déjà
#'
#' @return liste: spatRaster et table
#' @export
#'
#'

cv_carto_evo_frt <- function(shp = oiseauData::data_conf("shp"),
                             mnh_ts = terra::rast(oiseauData::data_conf("path_mnh_ts")),
                             an0 = oiseauData::data_conf("an0"),
                             an1 = oiseauData::data_conf("an1"),
                             lim_h_rege = oiseauData::data_conf("lim_h_rege"),
                             lim_h_perche = oiseauData::data_conf("lim_h_perche"),
                             lim_dh_rege = oiseauData::data_conf("lim_dh_rege"),
                             lim_dh_perche = oiseauData::data_conf("lim_dh_perche"),
                             lim_dh_futaie = oiseauData::data_conf("lim_dh_futaie"),
                             max_acc = oiseauData::data_conf("max_acc"),
                             max_area_perche = oiseauData::data_conf("max_area_perche"),
                             seuil_dtpi = oiseauData::data_conf("seuil_dtpi"),
                             path_desserte = oiseauData::data_conf("dos_shp_desserte"),
                             champ_id = "id",
                             w =  oiseauData::data_conf("w"),
                             buffer =  oiseauData::data_conf("buffer"),
                             dos_evo = oiseauData::data_conf("dos_evo"),
                             path_mnt =  oiseauData::data_conf("path_mnt"),
                             force_recalc = FALSE){

  # if(!is.null(.dir)){
  #
  #
  #   mnh0 <- terra::rast(oiseauData::data_conf("path_mnh0"))
  #   mnh1 <- terra::rast(oiseauData::data_conf("path_mnh1"))
  #
  #   abs <- list()
  #
  #   for(arg in c("shp","an0", "an1", "lim_h_rege", "lim_h_perche",
  #                "lim_dh_rege", "lim_dh_perche", "lim_dh_futaie",
  #                "max_acc", "max_area_perche", "seuil_dtpi",
  #                "desserte", "w", "dos_projet", "dos_evo", "path_mnt", "buffer")){
  #     if(is.null(oiseauData::data_conf(arg))){
  #       abs[[length(abs) + 1]] <- arg
  #     }else{
  #       assign(arg, oiseauData::data_conf(arg))
  #     }
  #   }
  #
  #   if(length(abs)>0){
  #
  #     warning(paste("Les paramètres suivants ne sont pas configurés: ",
  #                   paste(unlist(abs), collapse = ", "),
  #                   "\nUtiliser la fonction oiseauData::data_conf('arg', set = maValeur)"))
  #     return(unlist(abs))
  #   }
  #
  #   # recalculer ou simplement charger evo ?
  #
  #   if(file.exists(dos_evo) & !force_recalc){
  #
  #     evo <- try(list(renouv = terra::rast(file.path(dos_evo, "renouv.tif")),
  #                     renouv100 = terra::rast(file.path(dos_evo, "renouv100.tif")),
  #                     futaie = terra::rast(file.path(dos_evo, "futaie.tif")),
  #                     futaie100 = terra::rast(file.path(dos_evo, "futaie100.tif"))
  #                     # param = readRDS(file.path(dos_evo, "param.rds"))
  #                     ),
  #                silent = TRUE)
  #
  #     if(inherits(evo, "try-error")){
  #       recalc <- TRUE
  #     }else{
  #
  #       recalc <- FALSE
  #       # les paramètres ont-ils changé ?
  #
  #       # compare_param <- purrr::map_lgl(names(evo$param), ~ evo$param[[.x]] != get(.x)) %>% sum
  #       #
  #       # if(compare_param > 0){
  #       #   recalc <- TRUE
  #       # }else{
  #       #   if(force_recalc){
  #       #     recalc <-TRUE
  #       #   }else{
  #       #     recalc <-FALSE
  #       #   }
  #       # }
  #     }
  #   }else{
  #     recalc <- TRUE
  #   }
  #
  #   if(! recalc){
  #     return(NULL)
  #   }else{
  #     if(exists("evo")) rm(evo)
  #   }
  # }

  desserte = tryCatch({
    sf::st_read(path_desserte)
  }, error = function(e){NULL})

  # (re)calcul des évolutions

  if((w %% 2) == 0) w <- w+1

  dos_tmp <- file.path(dirname(dos_evo), "tmp_evo")

  unlink(dos_tmp, recursive = TRUE)
  Sys.umask(0)
  dir.create(dos_tmp)

  parcelles <- unique(shp[[champ_id]]) %>% sort

  # groupes de parcelles

  inter0 <- sf::st_intersects(shp %>% sf::st_buffer(buffer))


  inter <- inter0
  i <- 1
  while(i < length(inter)){
    rw <- inter[[i]]
    for(j in 1:length(inter)){
      if(any(inter[[i]] %in% inter[[j]])){
        inter[[i]] <- inter[[j]] <- sort(unique(c(inter[[i]], inter[[j]])))
      }
    }
    i <- i+1
  }

  shp$ten <- purrr::map(inter, paste, collapse = "_") %>% unlist %>% as.factor() %>% as.numeric()

  # selection des MNH

  mnh0 <- mnh_ts %>% terra::subset(as.character(an0))
  mnh1 <- mnh_ts %>% terra::subset((as.character(an1)))

  # calcul pour chaque tènement

  pb <- progress::progress_bar$new(total = length(parcelles))

  purrr::map(unique(shp$ten), function(.x){

    pb$tick()

    message("\nparcelle ", .x)
    suppressMessages(suppressWarnings(
      try(
        suppressMessages(suppressWarnings(
          cv.carto_evo(
            shp %>% dplyr::filter(ten == .x) %>% sf::st_buffer(buffer),
            mnh0, mnh1,
            an0 = an0, an1 = an1,
            lim_h_rege = lim_h_rege, lim_h_perche = lim_h_perche,
            lim_dh_perche = lim_dh_perche, lim_dh_futaie = lim_dh_futaie,
            max_acc = max_acc,
            max_area_perche = max_area_perche,
            seuil_dtpi = seuil_dtpi,
            desserte = desserte,
            dos_dest = dos_tmp,
            champ_id = champ_id)
        ))
      )
    ))

    if(inherits(try, "try-error")){
      message("... impossible de créer la carte.")
    }
  })


  # futaie

  ls_futaie <- list.files(dos_tmp,
                          pattern = "futaie", full.names = TRUE)

  futaie <- terra::vrt(ls_futaie)

  # renouv

  ls_renouv <- list.files(dos_tmp,
                          pattern = "renouv", full.names = TRUE)

  renouv <- terra::vrt(ls_renouv)


  evo <- list(
    renouv = renouv,
    futaie = futaie
  )

  evo$renouv100 <- evo$renouv %>% terra::aggregate(10, "modal")
  evo$futaie100 <- evo$futaie %>% terra::aggregate(10, "modal")
  evo$param <- list(lim_h_rege = lim_h_rege,
                    lim_h_perche = lim_h_perche,
                    lim_dh_rege = lim_dh_rege,
                    lim_dh_perche = lim_dh_perche,
                    lim_dh_futaie = lim_dh_futaie,
                    max_acc = max_acc,
                    seuil_dtpi = seuil_dtpi,
                    max_area_perche = max_area_perche)

  if(!dir.exists(dos_evo)){
    Sys.umask(0)
    dir.create(dos_evo)
  }

  crs <- terra::crs(terra::rast(path_mnt))
  for(n in names(evo)[names(evo) != "param"]){
    terra::crs(evo[[n]]) <- crs
  }

  terra::writeRaster(evo$renouv, file.path(dos_evo, "renouv.tif"), overwrite = TRUE)
  terra::writeRaster(evo$renouv100, file.path(dos_evo, "renouv100.tif"), overwrite = TRUE)
  terra::writeRaster(evo$futaie, file.path(dos_evo, "futaie.tif"), overwrite = TRUE)
  terra::writeRaster(evo$futaie100, file.path(dos_evo, "futaie100.tif"), overwrite = TRUE)



  return("OK")
}
