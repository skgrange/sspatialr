% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_geojson_js.R
\name{write_geojson_js}
\alias{write_geojson_js}
\title{Function to export an \code{sf} object as GeoJSON with a JavaScript variable 
name, usually for mapping with Leaflet.}
\usage{
write_geojson_js(sf, file, name = NA, pretty = TRUE, round = NA)
}
\arguments{
\item{sf}{A \strong{sf} object.}

\item{file}{File to export the \strong{sf} object to.}

\item{name}{The JavaScript variable name to give the GeoJSON object.}

\item{pretty}{Should the exported GeoJSON be pretty printed?}

\item{round}{Number of digits to round coordinates to.}
}
\value{
An invisible GeoJSON with JavaScript name character vector.
}
\description{
Function to export an \code{sf} object as GeoJSON with a JavaScript variable 
name, usually for mapping with Leaflet.
}
\author{
Stuart K. Grange
}
