#' catégorise et colore les rasters d'évolution
#'
#' @param r spatraster d'évolution du couvert
#' @param plot TRUE pour éditer une carte
#' @param shp (fac) polygones à ajouter à la carte
#'
#' @return spatraster
#' @export
#'

cv.color_evo <- function(r, plot = TRUE, shp = NULL){
  r <- terra::as.factor(r)

  cats <- cv_types_evo() %>% dplyr::select(cat, description, col) %>%
    dplyr::rename(ID = cat, category = description) %>%
    dplyr::mutate(category = stringr::str_replace_all(category, " ", "."),
                  category = paste(ID, category, sep = "."))
  levels(r) <- cats
  terra::cats(r,1)
  terra::coltab(r) <- terra::cats(r,1)$col

  if(plot){
    terra::plot(r, axes = FALSE,
                legend = "bottomleft",
                plg=list( # parameters for drawing legend
                  cex = .5, # Legend text size,
                  horiz = FALSE,
                  x.intersp = .5,
                  inset = -.1
                ))
    if(!is.null(shp)){
    terra::plot(shp %>% as("SpatVector"), add = T)
    }
  }

  r
}
