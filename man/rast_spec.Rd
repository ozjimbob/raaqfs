% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rast_spec.r
\name{rast_spec}
\alias{rast_spec}
\title{Return raster grid of variable for a time period}
\usage{
rast_spec(input, spec, hour)
}
\arguments{
\item{input}{An open ncdf4 object.}

\item{spec}{Full species name (4 chars) including spaces.}

\item{hour}{Hour (1-24) to return}
}
\value{
Raster grid of species concentration.
}
\description{
Return raster grid of variable for a time period
}
\examples{
library(ncdf4)
library(raster)
tfile <- system.file("extdata","aust_20140212_nat_0.800dg_morw.nc",package="raaqfs")
mydata <- nc_open(tfile)
tra <- rast_spec(mydata,"EC25",1) 
plot(tra)
library(maps)
map("world",add=TRUE)
}

