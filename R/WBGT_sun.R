#' WBGT_sun
#'
#' @param hurs relative humidity (%)
#' @param tas 2m air temperature (K)
#' @param ps air pressure (Pa)
#' @param ... individual .nc file name of climate variables in order
#' This function is from Dunne et al (2013)
#' @importFrom raster stack overlay calc stackApply as.matrix ncell nlayers
#' @importFrom dplyr %>%
#'
#' @return A raster stack of daily grid-level heat stress level
#' @export
#'
#' @examples
#' \dontrun{
#' WBGT.sun <- WBGT_sun(50, 303.15, 100000)
#' expect WBT = 22.25
#' expect WBGT.shade = 24.57
#' }
WBGT_sun <- function(hurs, tas, ps){
  ps = ps / 100 # transform Pa to mbar
  e_sat = exp(-2991.2729/((tas)^2) - 6017.0128/tas
              + 18.87643854 - 0.028354721 * tas
              + 1.7838301 * 10^(-5) * (tas)^2 - 8.4150417 * 10^(-10) * (tas)^3
              + 4.4412543 * 10^(-13) * tas^4 + 2.858487 * log(tas))/100
  w_sat = 621.97 * e_sat / (ps - e_sat)
  w = hurs/100*w_sat
  T_L = 1/(1/(tas - 55) - log(hurs/100)/2840) + 55
  the_E = tas*(1000/ps)^(0.2854*(1-0.28*0.001*w)) * exp((3.376/T_L - 0.00254) * w * (1+0.81*0.001*w))
  WBT = 45.114 - 51.489 * (the_E/273.15)^(-3.504)
  WBGT_shade = 0.7 * WBT + 0.3 * (tas -273.15)
  WBGT_sun = WBGT_shade + 3
  return(WBGT_sun)
}
