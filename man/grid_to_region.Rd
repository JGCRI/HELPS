% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_to_region.R
\name{grid_to_region}
\alias{grid_to_region}
\title{grid_to_region}
\usage{
grid_to_region(grid_annual_value, SECTOR, rast_boundary)
}
\arguments{
\item{grid_annual_value}{data.frame of grid-level annual value}

\item{SECTOR}{choice of sector}

\item{rast_boundary}{rasterized regional boundaries, 0.5 degree, Set to global extent}
}
\value{
A sf data frame of regional annual value
}
\description{
Aggregate Grid-Level Annual Values to Regional-Level
}
\details{
The \code{grid_to_region} function aggregates grid-level annual values to regional scales using spatial mapping
weights and rasterized regional boundaries. This allows for the computation of regional annual
values for a specified sector.
}
\examples{
\dontrun{
reg_annual_value <- grid_to_region(grid_annual_value = pwc.hothaps.ann, SECTOR = "MAIZ_R", rast_boundary = country_raster)
}
}
