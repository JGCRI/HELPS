#' LHR_Foster
#'
#' @param WBGT web bulb globe temperature (C)
#' @param workload not required in this LHR, but keep it to be consistent with other LHRs
#' This function is from Foster et al (2021) Table 3
#' An advanced empirical model for quantifying the impact of heat and climate change on human physical work capacity
#' adjustment in this package: PWC = 1 for negative WBGT
#' For additional scientific adjustments, see Smallcombe et al. (2022) Table 2
#' https://link.springer.com/article/10.1007/s00484-022-02370-7
#'
#' @return value of physical work capacity, range from 0 to 1
#' @export
#'
#' @examples
#' \dontrun{
#' PWC.Foster.Ag <- LHR_Foster(WBGT = 27)
#' }
LHR_Foster <- function(WBGT, workload = NULL){
  PWC = 1 / (1 + (33.63/max(0,WBGT))^-6.33)
  return(PWC)
}
