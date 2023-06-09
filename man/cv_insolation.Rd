% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_insolation.R
\name{cv_insolation}
\alias{cv_insolation}
\title{Insolation reçue à la hauteur du seuil de la strate basse}
\usage{
cv_insolation(
  dos_proj = oiseauData::data_conf("dos_projet"),
  path_mnt = oiseauData::data_conf("path_mnt"),
  path_mnh = oiseauData::data_conf("path_mnh_ts"),
  shp = oiseauData::data_conf("shp"),
  lim_h_rege = oiseauData::data_conf("lim_h_rege")
)
}
\arguments{
\item{conf}{conf}

\item{mnh}{liste de mnh}
}
\value{
liste: rasters des 2 années + leaflet + table id + ggplot
}
\description{
Insolation reçue à la hauteur du seuil de la strate basse
}
