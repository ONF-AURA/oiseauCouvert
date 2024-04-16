#' Raster Insolation au 15/07
#'
#' @param mne MNE = MNT + MNH
#'
#' @return raster
#' @export
#'
#' @import insol
#'

cv.sun <- function(mne, month = 7){

  # dem <- lid_spat2rast(mnt + mnh1)

cgr <- insol::cgrad(mne)
demm <- raster::as.matrix(mne)
dl <- raster::res(mne)[1]

## Isolation at 2 h interval over the length of the day
## RH and temp would cahnge over the dy, here we use a constant value for simplicity

height <- raster::quantile(mne, .5, na.rm = TRUE)
visibility <- 30
RH <- 80
tempK <- 288
tmz <- 0
year <- 2022
# month <- 7
day <- 21
timeh <- 12
jd <- insol::JDymd(year,month,day,hour = timeh)
Iglobal <- array(0,dim = dim(demm))
deltat <- 2 #heures

coo  <-  sf::st_point(apply(raster::bbox(mne), 1, mean)) %>%
  sf::st_sfc(crs = 2154) %>%
  sf::st_transform(4326) %>% sf::st_coordinates()

lat <- coo[2]
lon <- coo[1]

dayl <- insol::daylength(lat,lon,jd,0)

for (srs in seq(dayl[1],dayl[2],deltat)){
  message("heure: ", srs)
   jd <- insol::JDymd(year,month,day,hour = srs)
   sv <- insol::sunvector(jd,lat,lon,tmz)
   hsh <- insol::hillshading(cgr,sv)
   sh <- insol::doshade(demm,sv,dl)
   zenith <- insol::sunpos(sv)[2]
   Idirdif  <-  insol::insolation(zenith,jd,height,visibility,RH,tempK,0.002,0.15)
   ## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
     ## values in J/m^2
     Iglobal  <-  Iglobal + (Idirdif[,1] * hsh + Idirdif[,2] )*3600*deltat
     }


## rasterize to plot nicely
 Iglobal <- raster::raster(Iglobal,crs = raster::projection(mne))
 raster::extent(Iglobal) <- raster::extent(mne)

 Iglobal

}
