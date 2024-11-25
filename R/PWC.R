#' PWC
#'
#' @param WBGT output from HeatStress function
#' @param LHR choice of labor heat response function
#'
#' @importFrom raster stack overlay calc stackApply as.matrix ncell nlayers
#' @importFrom dplyr %>%
#'
#' @return A raster stack of daily grid-level physical work capacity multiplier: ranges from 0 to 1
#' @export
#'
#' @examples
#' \dontrun{
#' GD_PWC <- PWC(HeatStress = HS.stack.y,  LHR = LHR1)
#' }

PWC <- function(WBGT, LHR){
  PWC.stack.y <- stack(lapply(1:nlayers(WBGT), function(nlay) {calc(WBGT[[nlay]], LHR)})) # 19s
  layer_names <- names(WBGT)
  names(PWC.stack.y) <- layer_names
  return(PWC.stack.y)
}
