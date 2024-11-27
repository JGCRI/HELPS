#' LHR_NIOSH
#'
#' @param WBGT relative humidity (%)
#' @param workload 'high', 'moderate', 'low', specifying the rate of metabolic heat production (Watts)
#' This function is from Bröde et al (2018)
#' Choose National Institute for Occupational Safety and Health (NIOSH) standard; (Jacklitsch et al. 2016)
#' Based on Kjellstrom et al (2009):
#' M = 200W for office desk work and service industries
#' M = 300W for average manufacturing industry work
#' M = 400W for construction or agricultural work
#' M = 117W for resting
#'
#' @return value of physical work capacity, range from 0 to 1
#' @export
#'
#' @examples
#' \dontrun{
#' PWC.NIOSH.Ag <- LHR_NIOSH(WBGT = 27, M = 400)
#' }
LHR_NIOSH <- function(WBGT, workload){
  if(workload == "high"){
    M = 400
  } else if(workload == "moderate"){
    M = 300
  } else if(workload == "low"){
    M = 200
  } else {
    stop("Error: check input for workload: 'high', 'moderate', 'low'")
  }
  WBGT_lim = 56.7 - 11.5*log10(M)
  WBGT_lim_rest = 56.7 - 11.5*log10(117)
  level = min(1, (WBGT_lim_rest - WBGT)/(WBGT_lim_rest - WBGT_lim))
  PWC = max(0, level)
  return(PWC)
}
