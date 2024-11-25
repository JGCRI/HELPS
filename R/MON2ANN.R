#' MON2ANN
#'
#' @param input_rack Raster rack of monthly value
#' @param SECTOR choice of sector
#'
#' @importFrom raster stack overlay calc stackApply as.matrix as.data.frame ncell nlayers
#' @importFrom dplyr %>%
#'
#' @return A data frame of grid-level annual value
#' @export
#'
#' @examples
#' \dontrun{
#' annual_PWC <- MON2ANN(input_rack = month_PWC, SECTOR = "MAIZ_I")
#' annual_HS <- MON2ANN(input_rack = month_HS, SECTOR = "MAIZ_I")
#' }

MON2ANN <- function(input_rack, SECTOR){
  SECTOR_INDEX <- which(SECTOR_ALL == SECTOR)
  month_weight <- SECTOR_MONTH_WEIGHT[[SECTOR_INDEX]]
  input_mtx <- as.matrix(input_rack) * month_weight
  annual_mean <- apply(input_mtx, 1, function(row) if (all(is.na(row))) NA else sum(row, na.rm = TRUE))
  annual_output <- as.data.frame(input_rack, xy = T) %>% dplyr::select(x, y)
  annual_output$value <- annual_mean
  return(annual_output)
}
