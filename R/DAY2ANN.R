#' DAY2ANN
#'
#' @param CROP_IRR choice of crop and irrigation practice
#' @param WBGT choice of heat stress function
#' @param ETA choice of labor-heat response function
#' @param YEAR_INPUT a vector of years of interest
#' @param ... individual .nc file name of climate variables in order
#'
#' @import ncdf4
#' @importFrom raster stack overlay calc stackApply as.matrix ncell nlayers
#' @importFrom dplyr %>%
#'
#' @return A list of two data frames of annual gridded WBGT and PWC
#' @export
#'
#' @examples
#' \dontrun{
#' annual_output <- DAY2ANN(CROP_IRR = "MAIZ_I", WBGT = WBGT, ETA = ETA, YEAR_INPUT = 2027,
#' "hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#' "tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#' "ps_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
#' }




DAY2ANN <- function(CROP_IRR, WBGT, ETA, YEAR_INPUT = NULL, ...){

  CROP_INDEX <- which(CROP == CROP_IRR)
  SPAM_INDEX <- which(SPAM_CROP == CROP_IRR)
  crop_calendar <- CALENDAR_MONTH[[CROP_INDEX]]
  HA_filter <- SPAM_HA_FILTER[[CROP_INDEX]]

  # TODO:
  # add if else for WBGT and ETA
  # is.null(WBGT) then use default choices
  # is.null(ETA) then use default choices

  # check if the length of ISIMIP climate files input is identical to WBGT input variable
  # Extract additional arguments as a list
  climate_vars <- list(...)
  wbgt_arg_count <- length(formals(WBGT))
  if (length(climate_vars) != wbgt_arg_count) {
    stop("The number of climate variables does not match the WBGT function's requirements.")
  }

  # read in individual files of climate variables
  for (i in seq_along(climate_vars)) {
    assign(paste0("var", i,".stack"), raster::stack(climate_vars[[i]]))
  }

  layer_names <- names(var1.stack)
  layer_dates <- as.Date(gsub("X", "", layer_names), format = "%Y.%m.%d")
  date_range <- range(layer_dates)
  start_date <- date_range[1]  # Replace with the actual start date of your data
  end_date <- date_range[2]
  dates <- seq.Date(start_date, end_date, by = "day")
  year <- format(dates, "%Y")
  years <- as.integer(year)
  unique_years <- unique(as.vector(years))
  print(unique_years)

  if (is.null(YEAR_INPUT)) {
    YEAR_INPUT <- unique_years # include all available years in the original file
  } else if (all(YEAR_INPUT %in% unique_years)) {
    YEAR_INPUT <- YEAR_INPUT
    # If YEAR_INPUT is a subset of unique_years, proceed with the given YEAR_INPUT
  } else {
    # If X is not a subset of Y, stop with an error
    stop("Error: invalid YEAR_INPUT, make sure input files include data for all years specified in YEAR_INPUT")
  }

  ANNUAL_WBGT <- matrix(NA, nrow = ncell(var1.stack), ncol = length(YEAR_INPUT))
  ANNUAL_ETA <- matrix(NA, nrow = ncell(var1.stack), ncol = length(YEAR_INPUT))
  REGIONAL_ANNUAL <- list()

  for (y in 1:length(YEAR_INPUT)){
    YOI <- YEAR_INPUT[[y]]
    # print(YOI)
    layer_names_y <- layer_names[which(years == YOI)]
    variable_list <- list()
    for (i in 1:length(formals(WBGT))) {
      # assign SPAM HA area filter
      assign(paste0("var", i, ".SPAM.y"), get(paste0("var", i, ".stack"))[[which(years == YOI)]] * HA_filter)
      variable_list[[i]] <- get(paste0("var", i, ".SPAM.y"))
    }
    # Use do.call to overlay the climate variable list
    WBGT.stack.y <- do.call(overlay, c(variable_list, fun = WBGT))
    names(WBGT.stack.y) <- layer_names_y
    WBGT.stack.y <- stack(WBGT.stack.y)
    # TODO: how to make this step faster
    ETA.stack.y <- stack(lapply(1:nlayers(WBGT.stack.y), function(nlay) {calc(WBGT.stack.y[[nlay]], LHR)})) # 19s
    names(ETA.stack.y) <- layer_names_y
    layer_dates_y <- as.Date(gsub("X", "", layer_names_y), format = "%Y.%m.%d")
    date_range_y <- range(layer_dates_y)
    dates_y <- seq.Date(date_range_y[1], date_range_y[2], by = "day")
    month_y <- format(dates_y, "%m")
    months_y <- as.integer(month_y)
    unique_months_y <- unique(as.vector(months_y))
    MONTH_WBGT <- list()
    MONTH_ETA <- list()
    for (m in 1:length(unique_months_y)){
      MOI <- unique_months_y[[m]]
      month_WBGT <- WBGT.stack.y[[which(months_y == MOI)]]
      month_mean_WBGT <- stackApply(month_WBGT, 1, fun = mean, na.rm = TRUE)
      MONTH_WBGT[[m]] <- month_mean_WBGT
      month_ETA <- ETA.stack.y[[which(months_y == MOI)]]
      month_mean_ETA <- stackApply(month_ETA, 1, fun = mean, na.rm = TRUE)
      MONTH_ETA[[m]] <- month_mean_ETA
    }
    month_WBGT_stack <- stack(MONTH_WBGT)
    month_ETA_stack <- stack(MONTH_ETA)

    month_WBGT_mtx <- raster::as.matrix(month_WBGT_stack)
    month_ETA_mtx <- raster::as.matrix(month_ETA_stack)

    # apply the crop calendar weight matrix, the rowSums of weight = 1,
    # so use the rowSums to calculate the annual mean

    month_WBGT_mtx_calendar <-  month_WBGT_mtx * crop_calendar
    annual_WBGT_mean <- apply(month_WBGT_mtx_calendar, 1, function(row) {
      if (all(is.na(row))) {
        return(NA)  # Return NA if the entire row is NA
      } else {
        return(sum(row, na.rm = TRUE))  # Sum while ignoring NA
      }
    })

    month_ETA_mtx_calendar <- month_ETA_mtx * crop_calendar
    annual_ETA_mean <- apply(month_ETA_mtx_calendar, 1, function(row) {
      if (all(is.na(row))) {
        return(NA)  # Return NA if the entire row is NA
      } else {
        return(sum(row, na.rm = TRUE))  # Sum while ignoring NA
      }
    })
    ANNUAL_WBGT[, y] <- annual_WBGT_mean
    ANNUAL_ETA[, y] <- annual_ETA_mean
  }
  WBGT.output <- cbind(Coordinates_template, ANNUAL_WBGT) %>% setNames(c("x", "y", paste0("WBGT_",YEAR_INPUT)))
  ETA.output <- cbind(Coordinates_template, ANNUAL_ETA) %>% setNames(c("x", "y", paste0("ETA_",YEAR_INPUT)))
  list(WBGT.output, ETA.output)
}
