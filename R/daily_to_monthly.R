#' daily_to_monthly
#'
#' Convert Daily Raster Data to Monthly Averages
#'
#' This function takes a raster stack of daily values and computes the monthly averages for each pixel.
#'
#' @param input_rack Raster rack of daily value
#'
#' @importFrom raster stack overlay calc stackApply as.matrix ncell nlayers
#' @importFrom dplyr %>%
#'
#' @return A raster rack of monthly value
#' @export
#'
#' @examples
#' \dontrun{
#' month_PWC <- daily_to_monthly(input_rack = PWC.stack.y)
#' month_HS <- daily_to_monthly(input_rack = HS.stack.y)
#' }
daily_to_monthly <- function(input_rack) {
  # Create a vector of months corresponding to each layer
  layer_names <- names(input_rack)
  layer_dates <- as.Date(gsub("X", "", layer_names), format = "%Y.%m.%d")
  MONTHS <- format(layer_dates, "%m")
  # Calculate monthly mean using stackApply
  monthly_mean <- stackApply(input_rack, indices = MONTHS, fun = mean)
  monthly_mean <- stack(monthly_mean)
  return(monthly_mean)
}
