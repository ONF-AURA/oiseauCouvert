#' Cartographie des différences entre 2 mnh de dates différntes
#'
#' @param shp_ug polygone sf
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
#' @param desserte sf des dessertes avec champ 'largeur'
#' @param champ_id champ d'identifiant unique de shp,
#' @param seuil_dtpi différence de TPI au-delà duquel le pixel est considéré comme en fermeture latérale du couvert
#' @param dos_dest dossier d'écriture des tifs
#'
#' @return liste: raster et table
#' @export
#'
cv.carto_evo <- function(shp_ug, mnh0, mnh1, an0, an1,
                         lim_h_rege = 3, lim_h_perche = 15,
                         lim_dh_rege = 0.2, lim_dh_perche = 0.5, lim_dh_futaie = 0.3,
                         max_acc = 1,
                         max_area_perche = 30,
                         seuil_dtpi = 2,
                         desserte = NULL,
                         champ_id,
                         dos_dest,
                         id_dest){

  print(paste("seuil_dtpi = ", seuil_dtpi))
  h.temp <- terra::crop(mnh0, as(shp_ug, "SpatVector"))

  msk <- terra::rasterize(as(shp_ug, "SpatVector"), h.temp)

  h1 <- terra::mask(terra::crop(mnh1, shp_ug), msk)
  h0 <- terra::mask(terra::crop(mnh0, shp_ug), msk)
  h1[h1<0] <- 0
  h0[h0<0] <- 0

  hd <- h1-h0

  dif_tpi <- terra::terrain(h0, "TPI") - terra::terrain(h1, "TPI")

  dif_tpi[is.na(dif_tpi)] <- 0

  types <- data_typo_evo()

  # fonction de détermination de la catégorie ------------------------------------

  set_cat <- function(h0, h1, dtpi, area, carte_renouv = FALSE){

    acc <- (h1-h0)/(an1-an0)
    acc_excessif <- (acc > max_acc)
    acc_rege_faible <- (acc < lim_dh_rege)
    acc_perche_faible <- (acc < lim_dh_perche)
    acc_futaie_faible <- (acc < lim_dh_futaie)
    dtpi_excessif <- (dtpi > seuil_dtpi)
    rege0 <- (h0 <= lim_h_rege)
    rege1 <- (h1 <= lim_h_rege)
    perche0 <- (h0 > lim_h_rege & h0 <= lim_h_perche)
    perche1 <- (h1 > lim_h_rege & h1 <= lim_h_perche)
    futaie0 <- (h0 > lim_h_perche)
    futaie1 <- (h1 > lim_h_perche)
    chetif <- (area > max_area_perche & acc_perche_faible)

    if(carte_renouv){

      # CARTE REGE

      dplyr::case_when(

        is.na(h0) | is.na(h1) ~ as.numeric(NA),

        (h1 == 0 & h0 == 0) ~ 99,

        # rege0 -->

        rege0 ~ dplyr::case_when(
          rege1 ~ 3, # non régénéré
          perche1 & (acc_excessif | dtpi_excessif) ~ 5, # perches recouvrent régé
          futaie1 & (acc_excessif | dtpi_excessif) ~ 6, # futaies recouvrent régé
          acc_rege_faible ~ 3, # non régénéré
          acc_excessif ~ 5,
          TRUE ~ 4 # régénéré
        ),

        # perches0 -->

        perche0 ~ dplyr::case_when(
          rege1 ~ 2, # ouvert
          futaie1 & (acc_excessif | dtpi_excessif) ~ 14, # futaies recouvrent perches
          futaie1 ~ 19, # passage à la futaie
          # chetif ~ 18, # peuplement chétif
          TRUE ~ 10 # perchis
        ),

        # futaie0 -->

        futaie0 ~ dplyr::case_when(
          rege1 ~ 1, # ouvert
          perche1 ~ 18, # récolte sur perchis
          TRUE ~ 20, # futaie
        ),
        TRUE ~ 99
      )

    }else{

      # CARTE FUTAIE

      dplyr::case_when(

        is.na(h0) | is.na(h1) ~ as.numeric(NA),

        (h1 == 0 & h0 == 0) ~ 99,

        # rege0 -->

        rege1 ~ dplyr::case_when(
          futaie0 ~ 1, # ouverture
          perche0 ~ 2, # ouverture
          TRUE ~ 91
        ),

        perche1 ~ dplyr::case_when(
          chetif ~ 17, # peuplement chétif
          acc_perche_faible ~ 11,
          TRUE ~ 12
        ),

        futaie1 ~ dplyr::case_when(
          acc_futaie_faible ~ 21,
          perche0 ~ 19,
          TRUE ~ 22
        ),

        TRUE ~ 99
      )
    }
  }



  # 1. comparaison de pixel à pixel --------------------------------------

  print("1.pixels")

  # table des valeurs

  data <- data.frame(
    terra::values(c(h0, h1, dif_tpi))
  )

  names(data) <- c("h0", "h1", "dtpi")


  data <- data %>% dplyr::mutate(cat = set_cat(h0, h1, dtpi, area = 0, carte_renouv = TRUE))

  r_renouv_ini <- h1
  terra::values(r_renouv_ini) <- data$cat


  # 2. comparaison houppiers --------------------------

  # houppiers

  print("2.houppiers")

  ttops1 <- cv.apex(h1, lim_h_rege)


  # suppression des pixels de recouvrements (leur hauteur biaise la moyenne)

  # cat_recou <- types %>% filter(futaie_fill_renouv ==0) %>% pull(cat)
  # h1_nc <- h1
  #
  # values(h1_nc)[values(r_renouv_ini) %in% cat_recou] <- values(h0)[values(r_renouv_ini) %in% cat_recou]
  #
  # h1r_nc <- lid_spat2rast(h1_nc)
  # crs(h1r_nc) <- crs(ttops1)
  #
  # algo_nc <- lidR::dalponte2016(h1r_nc, ttops1)
  # crowns1_nc <- algo_nc()
  #
  #
  # crowns_nc <- suppressWarnings(
  #   st_as_stars(crowns1_nc) %>%
  #     st_as_sf(as_points = FALSE, merge = TRUE) %>%
  #     mutate(h1 = ttops1$Z,
  #            h0 = terra::extract(h0, as(ttops1, "SpatVector"))[[2]],
  #            dtpi = terra::extract(
  #              as(dif_tpi, "SpatRaster"),
  #              as(ttops1, "SpatVector")
  #            )[[2]],
  #            area = st_area(.) %>% as.numeric(),
  #            type = set_cat(h0, h1, dtpi, area, carte_renouv = FALSE)
  #     )
  # )
  #
  # cr_nc <- terra::rasterize(as(crowns_nc, "SpatVector"), r_renouv_ini, "type")
  #

  # véritables houppiers avec valeur des houppiers tronqués

  # h1r <- h1
  # h1r <- util_spat2rast(h1)
  # raster::crs(h1r) <- raster::crs(ttops1)
  #
  # algo <- lidR::dalponte2016(h1r, ttops1)
  # crowns_id<- algo()

  crowns_id <- data_crowns() %>% crop(h1)

  # suppression des pixels de recouvrement

  types_rec <- types %>% dplyr::filter(origine=="rec") %>% dplyr::pull(cat)
  crowns_sans_rec <- crowns_id
  crowns_sans_rec[terra::values(r_renouv_ini) %in% types_rec] <- NA

  # caracteristiques des houppiers

  type_crowns <- data.frame(
    id = raster::values(crowns_sans_rec),
    h0 = raster::values(h0 %>% util_spat2rast()),
    h1 = raster::values(h1 %>% util_spat2rast())
  ) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      area = dplyr::n(),
      h0 = max(h0, na.rm = TRUE),
      h1 = max(h1, na.rm = TRUE),
      type = set_cat(h0, h1, 0, area, carte_renouv = FALSE)
    )
  val_raster_crowns <- data.frame(
    id = raster::values(crowns_id)
  ) %>% dplyr::left_join(type_crowns) %>%
    dplyr::mutate(type = ifelse(is.na(id), as.numeric(NA), type))


  crowns_raster_0 <-  raster::as.factor(crowns_id)
  raster::values(crowns_raster_0) <- val_raster_crowns %>% dplyr::pull(type)

  r_futaie <- terra::focal(crowns_raster_0, 3, "modal", na.rm = TRUE) %>% terra::as.factor()



  # 3. corrections -------------------------------------

  print("3.corrections")

  # couche RENOUVELLEMENT -------------------------------------

  # utiliser les pixels de "futaie" pour les stades perches et futaie en simplifiant les codes

  df_modif_renouv <- data.frame(cat = terra::values(r_futaie)[,1]) %>%
    dplyr::left_join(types %>% dplyr::select(cat, futaie_fill_renouv), by = "cat")

  r_modif_renouv <- r_futaie

  val_modif_renouv <- df_modif_renouv$futaie_fill_renouv
  val_renouv_ini <- terra::values(r_renouv_ini)[,1]

  # conserver les pixels au stade régé et les pixels de recouvrement

  keep <- types %>% dplyr::filter(is.na(futaie_fill_renouv)) %>% dplyr::pull(cat)

  val_modif_renouv[which(val_renouv_ini %in% keep)] <- NA
  val_modif_renouv[which(val_modif_renouv == 0)] <- NA

  terra::values(r_modif_renouv) <- val_modif_renouv

  val_renouv <- val_renouv_ini
  val_renouv[which(!is.na(val_modif_renouv))] <-
    val_modif_renouv[which(!is.na(val_modif_renouv))]

  # remplace les types perches et futaies par les types simples

  rep <- types %>% dplyr::select(cat, futaie_fill_renouv) %>%
    dplyr::mutate(new_cat = ifelse(is.na(futaie_fill_renouv), cat, futaie_fill_renouv))

  val_renouv <- data.frame(cat = val_renouv_ini %>% as.numeric()) %>%
    dplyr::left_join(rep) %>% dplyr::pull(new_cat)

  r_renouv <- r_renouv_ini
  terra::values(r_renouv) <- val_renouv



  #  couche FUTAIE ---------------------------------

  # simplifier les stades régés par le code générique régénération

  val_modif_futaie <- data.frame(cat = val_renouv %>% as.numeric) %>%
    dplyr::left_join(types %>%
                       dplyr::select(cat, renouv_fill_futaie), by = "cat")

  modif_futaie <- r_renouv
  val_modif_futaie <- val_modif_futaie$renouv_fill_futaie

  val_futaie <- terra::values(r_futaie)[,1]
  val_futaie[which(!is.na(val_modif_futaie))] <-
    val_modif_futaie[which(!is.na(val_modif_futaie))]

  # remplace les types régé par les types simples

  rep <- types %>% dplyr::select(cat, renouv_fill_futaie) %>%
    dplyr::mutate(new_cat = ifelse(is.na(renouv_fill_futaie), cat, renouv_fill_futaie))

  val_futaie <- data.frame(cat = val_futaie %>% as.numeric()) %>%
    dplyr::left_join(rep) %>% dplyr::pull(new_cat)

  terra::values(r_futaie) <- val_futaie

  r_futaie <- terra::focal(r_futaie, 5, "modal", na.rm = TRUE)

  # 4.masque desserte

  if(! is.null(desserte)){

    print("4.desserte")

    if(is.null(desserte$largeur)){
      desserte$largeur <- 3
    }

    desserte_ug <- desserte %>%
      dplyr::filter(largeur > 0) %>%
      sf::st_crop(shp_ug)

    if(nrow(desserte_ug) > 0){

      dess_buff <- purrr::map(unique(desserte_ug$largeur),
                              ~ sf::st_buffer(
                                desserte %>% dplyr::filter(largeur == .x),
                                .x)
      )
      dess_buff <- do.call(rbind, dess_buff)

      ras_dess <- terra::rasterize(as(dess_buff, "SpatVector"), r_renouv, 1)

      val_dess <- terra::values(ras_dess)[,1]
      val_renouv <- terra::values(r_renouv)[,1]
      val_futaie <- terra::values(r_futaie)[,1]


      val_dess[val_renouv != 1] <- NA
      val_dess[is.na(val_renouv)] <- NA

      val_renouv[val_dess == 1] <- 99
      val_futaie[val_dess == 1] <- 99

      terra::values(r_renouv) <- val_renouv
      terra::values(r_futaie) <- val_futaie
    }
  }

  # val_fin_types <- data.frame(cat = as.numeric(terra::values(fin))) %>%
  #   group_by(cat) %>%
  #   summarise(area = n()/10000) %>%
  #   left_join(types)
  #
  # val_fin <- do.call(
  #   rbind,
  #   terra::extract(fin, as(shp_ug, "SpatVector"))
  # ) %>% t %>%
  #   as.data.frame() %>%
  #   group_by(ID, Z) %>%
  #   count() %>%
  #   mutate(prf = shp_ug$ug[ID],
  #          cat = Z,
  #          area = n) %>%
  #   left_join(types, by = "cat") %>%
  #   as.data.frame() %>%
  #   dplyr::select(-c(ID, Z, n))



  # 6- FOCAL

  # renouv_rege <- r_renouv
  # renouv_rege[renouv_rege %in% c(10,20)] <- NA
  # renouv_rege <- terra::focal(renouv_rege, 5, "modal")
  # renouv_rege[is.na(renouv_rege)] <- 0
  #
  # renouv_futaie <- r_renouv
  # renouv_futaie[! renouv_futaie %in% c(10,20)] <- NA
  # renouv_futaie <- terra::focal(renouv_futaie, 5, "modal")
  # renouv_futaie[is.na(renouv_futaie) | renouv_rege != 0] <- 0
  #
  # r_renouv <- renouv_rege + renouv_futaie
  # r_renouv[r_renouv == 0] <- NA
  #
  # lid_carto_evo_plot(c(r_renouv))

  terra::writeRaster(r_renouv,
                     file.path(dos_dest,
                               paste0("renouv", id_dest, ".tif")),
                     overwrite = TRUE)

  terra::writeRaster(r_futaie,
                     file.path(dos_dest,
                               paste0("futaie", id_dest, ".tif")),
                     overwrite = TRUE)
  raster::writeRaster(crowns_sans_rec,
                      file.path(dos_dest,
                                paste0("crowns", id_dest, ".tif")),
                      overwrite = TRUE)

}
