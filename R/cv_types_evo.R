#' table de la typologie d'évolution du couvert
#'
#' @return table
#' @export
#'
cv_types_evo <- function(){


  read.csv2(system.file("types.csv", package = "oiseauCouvert"),
            sep = ";", encoding = "latin1",
            stringsAsFactors = FALSE)

  # table des types ---------------------------
  #
  #   types <- data.frame(
  #     cat = c(1, 2, 3,
  #             4, 5, 6, 7,
  #             10, 11, 12,
  #             21, 22, 99),
  #     group = c(1001, 1001, 1003,
  #               1004, 1004, 1004, 1003,
  #               1010, 1010, 1012,
  #               1021, 1021, 99),
  #     type = c("non régénéré", "régénéré", "régé recouvert par perches",
  #              "ouvert", "relevé de couvert", "recolte", "refermé",
  #              "ppts peu productifs",  "ppts bas croissance lente", "ppts bas croissance rapide",
  #              "ppts haut croissance lente", "ppts haut croissance rapide", "autres"),
  #     col = c("red", "blue", "deeppink",
  #             "yellow", "orange","saddlebrown", "violet",
  #             "yellowgreen","palegreen", "springgreen", "forestgreen", "darkgreen", "gray"),
  #     col_txt = c("white","white","white",
  #                 "black", "black","white","white",
  #                 "black","black","black", "black", "white", "black"),
  #     description = c(
  #       "Zone déjà ouverte ne se refermant pas",
  #       "Zone ouverte en cours de fermeture par les accrus",
  #       "Zone ouverte en cours de fermeture par l'extension des houppiers voisins",
  #       "Zone ouverte autrefois fermée",
  #       "Zone où un sous-étage est mis en lumière",
  #       "Zone de disparition des tiges dominantes (récolte ou mortalité)",
  #       "Peuplement en cours d'etouffement par l'extension des houppiers voisins",
  #       "Peuplement peu vigoureux",
  #       "Peuplement jeune à croissance faible",
  #       "Peuplement jeune à croissance soutenue",
  #       "Peuplement adulte à croissance faible",
  #       "Peuplement adulte à croissance soutenue",
  #       "Surface non boisée"
  #     ),
  #     détermination = c(
  #       "H0 < lim_h_rege ET H1 < lim_h_rege ET acc < lim_dh_perche)",
  #       "H0 < lim_h_rege ET (H1 > lim_h_rege OU acc > lim_dh_perche)",
  #       "H0 < lim_h_rege ET (acc > max_acc OU dtpi > seuil_dtpi)",
  #       "H1 < lim_h_rege ET H0 > lim_h_rege",
  #       "H1 > lim_h_rege ET H0 > 0.5 H1",
  #       "H1 > lim_h_rege ET 0.99 H1 > H0 > 0.5 H1",
  #       "H0 > lim_h_rege ET (acc > max_acc OU dtpi > seuil_dtpi)",
  #       "H1 < lim_h_perche ET surface houppier > max_area_perche ET acc < lim_dh_perche",
  #       "H1 < lim_h_perche ET surface houppier < max_area_perche ET acc < lim_dh_perche",
  #       "H1 < lim_h_perche ET acc > lim_dh_perche",
  #       "H1 > lim_h_perche ET acc < lim_dh_perche",
  #       "H1 > lim_h_perche ET acc > lim_dh_perche",
  #       "H0 = H1 = 0 OU masque route")
  #
  #     )

}
