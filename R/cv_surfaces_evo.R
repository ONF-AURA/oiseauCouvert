#' Bilan des évolutions de surfaces de couverts
#'
#' @param conf conf
#' @param evo liste evo
#'
#' @return liste: table et leaflet
#' @export
#'

cv_surfaces_evo <- function(conf = readRDS(file.path(.dir, "conf.rds")), force = TRUE){


  ugs <- conf$shp$ug %>% unique %>% sort

  ls_surf_cvt <- map(ugs, function(u){
    message(u)
    lid_evo_couvert(u, conf, dest = conf$path_couvert_ug, force =  force)
  })

  recap <- do.call(rbind, ls_surf_cvt)

  types <- lid_types_evo()

  # surfaces par strates

  surf_strates <- recap %>%
    left_join(types %>% dplyr::select(cat, strate0, strate1, origine), by = "cat") %>%
    dplyr::select(ug, `ha m²`, strate0, strate1, origine) %>%
    filter(strate0 != "") %>% # types simplifiés
    tidyr::pivot_longer(c(strate0, strate1), names_to = "an", values_to = "strate") %>%
    mutate(an = str_sub(an, start = str_length(an)),
           origine = ifelse(an == "0", "", origine)) %>%
    group_by(ug, an, strate, origine) %>%
    summarise(surf = sum(`ha m²`, na.rm = TRUE)) %>%
    filter(!is.na(strate) & strate != "") %>%
    mutate(id = paste0(strate,"__", origine, "__", an)) %>%
    as.data.frame() %>%
    dplyr::select(-c(strate, an, origine)) %>%
    tidyr::pivot_wider(names_from = id, values_from = surf)


  bilan <- surf_strates %>%
    tidyr::pivot_longer(- ug, values_to = "surface") %>%
    mutate(strate = str_split(name, "__", simplify = TRUE)[,1],
           an = str_sub(name, str_length(name)),
           origine = str_split(name, "__", simplify = TRUE)[,2]) %>%
    dplyr::select(-name) %>%
    mutate(étage = factor(strate, levels = c("rege", "perchis", "futaie")))

  conf$bilan_evo <- bilan

  saveRDS(conf, file.path(conf$dos_proj, "conf.rds"))

  return(conf)

}
