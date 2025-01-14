% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PWC.R
\name{PWC}
\alias{PWC}
\title{PWC}
\usage{
PWC(WBGT, LHR, workload)
}
\arguments{
\item{WBGT}{output from HeatStress function}

\item{LHR}{choice of labor heat response function}

\item{workload}{choice of workload intensity, 'high', 'moderate', and, 'low'}
}
\value{
A raster stack of daily grid-level physical work capacity multiplier: ranges from 0 to 1
}
\description{
PWC
}
\examples{
\dontrun{
GD_PWC <- PWC(HeatStress = HS.stack.y,  LHR = LHR_Foster, workload = "high")
}
}
