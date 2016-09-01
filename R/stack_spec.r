#' Return raster stack of variable for a day.
#'
#' @param input An open ncdf4 object.
#' @param spec Full species name (4 chars) including spaces.
#' @return RasterStack of concentration.
#' @examples
#' library(ncdf4)
#' library(raster)
#' tfile <- system.file("extdata","aust_20140212_nat_0.800dg_morw.nc",package="raaqfs")
#' mydata <- nc_open(tfile)
#' salt <- stack_spec(mydata,"SS25") 
#' animate(salt,pause=0.1,n=2)
#' @export
stack_spec=function(input,spec){
  sl = get_spec_list(input)
  vno = which(sl %in% spec)
  if(length(vno)==0){
    stop(paste0("Chemical species ",spec," not found."))
  }
  g=list()
  
  
  lons=ncdf4::ncvar_get(input,"lon")
  lats=ncdf4::ncvar_get(input,"lat")
  lodiff=mean(diff(lons))
  ladiff=mean(diff(lats))
  for(hour in 1:24){
    odat=ncdf4::ncvar_get(nc=input,varid="cavg")
    odat=odat[,,vno,hour]
    rotm <- function(x) t(apply(x, 2, rev))
    odat=rotm(rotm(rotm(odat)))
    ra=raster::raster(odat,xmn=min(lons)-(lodiff/2),xmx=max(lons)+(lodiff/2),ymn=min(lats)-(ladiff/2),ymx=max(lats)+(ladiff/2))
    sp::proj4string(ra)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
  g[[hour]]=ra
  }
  g=raster::stack(g)
  g
}
