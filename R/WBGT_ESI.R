#' WBGT_ESI
#'
#' @param hurs relative humidity (%)
#' @param tas 2m air temperature (K)
#' @param sdsr surface downward solar radiation (W m-2)
#' @param ... individual .nc file name of climate variables in order
#' Environmental Stress Index (ESI) is from Moran et al (2001) & Moran et al (2003)
#'
#' @return A raster stack of daily grid-level heat stress level
#' @export
#'
#' @examples
#' \dontrun{
#' ESI.test <- WBGT_ESI(50, 303.15, 200)
#' }
WBGT_ESI <- function(hurs, tas, rsds){
  hurs = hurs / 100 # change percentage value to unitless value
  ESI = 0.62 * (tas - 273.15) - 0.7 * hurs + 0.002 * rsds +
  0.43 * (tas - 273.15) * hurs - 0.078 * (0.1+rsds)^(-1)
return(ESI)
}
