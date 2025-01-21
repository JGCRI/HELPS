#' cal_pwc
#'
#' Calculate Physical Work Capacity (PWC)
#'
#' The `cal_pwc` function computes daily grid-level physical work capacity (PWC),
#' which range from 0 to 1, based on heat stress levels, a chosen labor heat response function,
#' and workload intensity.
#'
#' @param WBGT output from HeatStress function
#' @param LHR choice of labor heat response function
#' @param workload choice of workload intensity, 'high', 'moderate', and, 'low'
#'
#' @importFrom raster stack overlay calc stackApply as.matrix ncell nlayers
#' @importFrom dplyr %>%
#'
#' @return A raster stack of daily grid-level physical work capacity multiplier: ranges from 0 to 1
#' @export
#'
#' @examples
#' \dontrun{
#' GD_PWC <- cal_pwc(HeatStress = HS.stack.y, LHR = LHR_Foster, workload = "high")
#' }
cal_pwc <- function(WBGT, LHR, workload) {
  PWC.stack.y <- stack(lapply(1:nlayers(WBGT), function(nlay) {
    calc(WBGT[[nlay]], function(x) LHR(x, workload))
  }))
  layer_names <- names(WBGT)
  names(PWC.stack.y) <- layer_names
  return(PWC.stack.y)
}
