#' LHR_ISO
#'
#' @param WBGT relative humidity (%)
#' @param workload 'high', 'moderate', 'low', specifying the rate of metabolic heat production (Watts)
#' This function is from Kjellstrom et al. (2014)
#' Based on Kjellstrom et al (2009):
#' M = 200W for office desk work and service industries
#' M = 300W for average manufacturing industry work
#' M = 400W for construction or agricultural work
#' M = 117W for resting
#' @return value of physical work capacity, range from 0 to 1
#' @export
#'
#' @examples
#' \dontrun{
#' PWC.ISO.Ag <- LHR_ISO(WBGT = 27, workload = 'high')
#' }
LHR_ISO <- function(WBGT, workload){
  if(workload == "high"){
    M = 400
  } else if(workload == "moderate"){
    M = 300
  } else if(workload == "low"){
    M = 200
  } else {
    stop("Error: check input for workload: 'high', 'moderate', 'low'")
  }
  WBGT_lim = 34.9 - M/46
  WBGT_lim_rest = 34.9 - 117/46
  level = min(1, (WBGT_lim_rest - WBGT)/(WBGT_lim_rest - WBGT_lim))
  PWC = max(0, level)
  return(PWC)
}
