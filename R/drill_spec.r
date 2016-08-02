#' Drill a timeseries of a variable at a given location
#'
#' @param input An open ncdf4 object.
#' @param spec Full species name (4 chars) including spaces.
#' @param lat Latitude of sample point.
#' @param lon Longitude of sample point
#' @return Timeseries vector of species concentration.
#' @examples
#' library(ncdf4)
#' library(raster)
#' tfile <- system.file("extdata","aust_20140212_nat_0.800dg_morw.nc",package="raaqfs")
#' mydata <- nc_open(tfile)
#' lat <- -33.5
#' lon <- 151.0
#' timeseries <- drill_spec(mydata,"EC25",lat,lon)
#' plot(timeseries,type="l",xlab="Hour",ylab="conc")
#' @export
drill_spec=function(input,spec,lat,lon){
  sl = get_spec_list(input)
  vno = which(sl %in% spec)
  if(length(vno)==0){
    stop(paste0("Chemical species ",spec," not found."))
  }
  odat=ncvar_get(nc=input,varid="cavg")
  lons=ncvar_get(input,"lon")
  lats=ncvar_get(input,"lat")
  if(lat < min(lat) | lat > max(lat) | lon < min(lon) | lon > max(lon)){
    stop("Coordinates provided are out of range of grid.")
  }
  tlon=which.min(abs(lons-lon))
  tlat=which.min(abs(lats-lat))
  odat=odat[tlon,tlat,vno,]
  odat
}
