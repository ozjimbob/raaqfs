#' Get species list from NetCDF file
#'
#' @param input An open ncdf4 object.
#' @return The vector of chemical species codes.
#' @examples
#' tfile <- system.file("extdata","aust_20140212_nat_0.800dg_morw.nc",package="raaqfs")
#' mydata <- nc_open(tfile)
#' species<-get_spec_list(mydata)
#' print(species)
#' @export
get_spec_list=function(input){
  spec=ncvar_get(nc=input,varid="name_fspec")
  nvar = nchar(spec)/4
  stseq=seq(from=1,to=nchar(spec),by=4)
  snpfn = function(st){substr(spec,st,st+3)}
  unlist(lapply(stseq,FUN=snpfn))
}