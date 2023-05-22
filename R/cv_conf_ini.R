#' Configuration par défaut d'un projet pour le calcul du couvert
#'
#' @param .dir dossier du projet
#' @param replace TRUE pour réinitialiser les paramètres
#'
#' @return
#' @export
#'
cv_conf_ini <- function(
    .dir = mget(".dir", envir = as.environment(1), ifnotfound = list(NULL))[[1]],
    replace = FALSE
){

  ls <- list(
    ls_ok = c(),
    ls_ko = c()
  )

  dc <- function(p, set, descr, ls0 = ls){
    if(!is.null(oiseauData::data_conf(p, silent = TRUE))){
      ls0$ls_ko <- c(ls0$ls_ko, p)
    }else{
      oiseauData::data_conf(p, set = set, process = "couvert", replace = TRUE, descr = descr, silent = TRUE)
      ls0$ls_ok <- c(ls0$ls_ok, p)
    }
    return(ls0)
  }

  ls <- dc("buffer", set = 20, descr = "distance du tampon appliqué aux limiites")
  ls <- dc("dos_mns", set = file.path(.dir, "mns"), descr = "chemin du dossier de travailpour la création du MNS issu du LiDARHD IGN")
  ls <- dc("path_mns", set = file.path(.dir, "mns", "mns.tif"), descr = "chemin du raster du MNS LiDARHD")
  ls <- dc("path_mnh0", set = file.path(.dir, "mnh0.tif"), descr = "chemin du raster du MNH de an0 du projet")
  ls <- dc("path_mnh1", set = file.path(.dir, "mnh1.tif"), descr = "chemin du raster du MNH de an1 du projet")
  ls <- dc("path_mnt", set = file.path(.dir, "mnt.tif"), descr = "chemin du raster du MNT du projet")
  ls <- dc("path_emprise_edf", set = file.path(.dir, "emprise_edf.tif"), descr = "chemin du raster des emprises des lignes électriques")

  ls <- dc("an0", set = 2014, descr = "année du MNH0")
  ls <- dc("an1", set = 2021, descr = "année du MNH1")
  ls <- dc("w", set = 5, descr = "largueur de la fenêtre d'aggrégation (impaire)")
  ls <- dc("path_ressources_ign", set = file.path(dc("dos_ign"),"ressources_web.csv"), descr = "chemin local du fichier de ressources IGN")

  ls <- dc("lim_h_rege", set = 3,
           descr = "seuil de hauteur entre les strates basse (régé) et intermédiaire (perches) - valeur suggérée: 3")
  ls <- dc("lim_h_perche", set = 15,
           descr = "seuil de hauteur entre les strates intermédiaire (perches) et haute (futaie) - valeur suggérée: 15")
  ls <- dc("lim_dh_rege", set = 0.2,
           descr ="Accroissement minimale annuel en hauteur pour qu'un pixel passant des strates basse à intermédiaire soint concidéré comme susceptible d'être régénéré (sinon, probabilité de fruticées) - valeur suggérée: 0.2")
  ls <- dc("lim_dh_perche", set = 0.5,
           descr ="seuil d'accroissement en hauteur pour juger d'une croissance forte ou faible de la strate intermédiaire - valeur suggéré: 0.5")
  ls <- dc("lim_dh_futaie", set = 0.3,
           descr ="seuil d'accroissement en hauteur pour juger d'une croissance forte ou faible de la strate supérieure - valeur suggéré: 0.3")
  ls <- dc("max_acc", set = 1,
           descr ="accroissement annuel en hauteur maximum, au-dessus duquel le différentiel de hauteur ne peut s'expliquer que par du recouvrement latéral - valeur suggérée: 1")
  ls <- dc("max_area_perche", set = 50,
           descr ="surface maximum d'une couronne de la strate intermédiaire susceptible de participer au renouvellement (au-dessus, préexistant, loup, pommier...")
  ls <- dc("seuil_dtpi", set = 2,
           descr ="différence de TPI au-delà duquel le pixel est considéré comme en fermeture latérale du couvert - valeur suggérée: 2")
  ls <- dc("dos_evo", set = file.path(.dir, "evo"),
           descr ="dossier des résulats de l'évolution du couvert")
  ls <- dc("path_evo_futaie", set = file.path(.dir, "evo", "futaie.tif"),
           descr ="raster 1m de l'évolution de la futaie")
  ls <- dc("path_evo_futaie100", set = file.path(.dir, "evo", "futaie100.tif"),
           descr ="raster 10m de l'évolution de la futaie")
  ls <- dc("path_evo_renouv", set = file.path(.dir, "evo", "renouv.tif"),
           descr ="raster 1m de l'évolution des strates inférieures")
  ls <- dc("path_evo_renouv100", set = file.path(.dir, "evo", "renouv100.tif"),
           descr ="raster 10m de l'évolution des strates inférieures")

  if(length(ls$ls_ok) > 0){
    message(length(ls$ls_ok), " paramètres ont été configurés par défaut: ", paste(ls$ls_ok, collapse = ", "))
  }

  if(length(ls$ls_ko) > 0){
    message(length(ls$ls_ko), " paramètres étaient déjà configurés et n'ont  pas été modifié: ", paste(ls$ls_ko, collapse = ", "))
  }

}
