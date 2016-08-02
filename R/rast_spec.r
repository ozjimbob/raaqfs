#' Return raster grid of variable for a time period
#'
#' @param input An open ncdf4 object.
#' @param spec Full species name (4 chars) including spaces.
#' @param hour Hour (1-24) to return
#' @return Timeseries vector of species concentration.
#' @examples
#' library(ncdf4)
#' library(raster)
#' tfile <- system.file("extdata","aust_20140212_nat_0.800dg_morw.nc",package="raaqfs")
#' mydata <- nc_open(tfile)
#' tra <- rast_spec(mydata,"EC25",1) 
#' plot(tra)
#' library(maps)
#' map("world",add=TRUE)
#' @export
rast_spec=function(input,spec,hour){
  sl = get_spec_list(input)
  vno = which(sl %in% spec)
  if(length(vno)==0){
    stop(paste0("Chemical species ",spec," not found."))
  }
  if(hour > 24){
    stop(paste0("Hour must be between 1 and 24"))
  }
  odat=ncvar_get(nc=input,varid="cavg")
  lons=ncvar_get(input,"lon")
  lats=ncvar_get(input,"lat")
  lodiff=mean(diff(lons))
  ladiff=mean(diff(lats))
  
  odat=odat[,,vno,hour]
  rotm <- function(x) t(apply(x, 2, rev))
  odat=rotm(rotm(rotm(odat)))
  ra=raster(odat,xmn=min(lons)-(lodiff/2),xmx=max(lons)+(lodiff/2),ymn=min(lats)-(ladiff/2),ymx=max(lats)+(ladiff/2))
  proj4string(ra)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
  ra
}
