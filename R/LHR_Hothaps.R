#' LHR_Hothaps
#'
#' @param WBGT web bulb globe temperature (C)
#' @param workload 'high', 'moderate', 'low'
#' This function is from Foster et al (2021)
#' For additional adjustment, see Smallcombe et al. (2022)
#'
#' @return value of physical work capacity, range from 0 to 1
#' @export
#'
#' @examples
#' \dontrun{
#' PWC.Hothaps.Ag <- LHR_Hothaps(WBGT = 27)
#' }
LHR_Hothaps <- function(WBGT, workload){
  if(workload == "high"){
    a1 = 30.94
    a2 = 16.64
  } else if(workload == "moderate"){
    a1 = 32.93
    a2 = 17.81
  } else if(workload == "low"){
    a1 = 34.64
    a2 = 22.72
  } else {
    stop("Error: check input for workload: 'high', 'moderate', 'low'")
  }
  PWC = 0.1 + 0.9 / (1 + (WBGT / a1)^a2)
  return(PWC)
}
