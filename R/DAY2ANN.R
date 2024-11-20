#' DAY2ANN
#'
#' @param CROP_IRR choice of crop and irrigation practice
#' @param HS choice of heat stress function
#' @param LHR choice of labor-heat response function
#' @param YEAR_INPUT a vector of years of interest
#' @param ... individual .nc file name of climate variables in order
#'
#' @importFrom raster stack overlay calc stackApply as.matrix ncell nlayers
#' @importFrom dplyr %>%
#'
#' @return A list of two data frames of annual gridded HS and PWC
#' @export
#'
#' @examples
#' \dontrun{
#' annual_output <- DAY2ANN(CROP_IRR = "MAIZ_I", HS = WBGT1, LHR = LHR1, YEAR_INPUT = 2027,
#' "hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#' "tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#' "ps_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
#' }




DAY2ANN <- function(CROP_IRR, HS, LHR, YEAR_INPUT = NULL, ...){

  CROP_INDEX <- which(CROP == CROP_IRR)
  SPAM_INDEX <- which(SPAM_CROP == CROP_IRR)
  crop_calendar <- CALENDAR_MONTH[[CROP_INDEX]]
  HA_filter <- SPAM_HA_FILTER[[CROP_INDEX]]

  # TODO:
  # add if else for HS and PWC
  # is.null(HS) then use default choices
  # is.null(PWC) then use default choices

  # check if the length of ISIMIP climate files input is identical to HS input variable
  # Extract additional arguments as a list
  climate_vars <- list(...)
  HS_arg_count <- length(formals(HS))
  if (length(climate_vars) != HS_arg_count) {
    stop("The number of climate variables does not match the HS function's requirements.")
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

  ANNUAL_HS <- matrix(NA, nrow = ncell(var1.stack), ncol = length(YEAR_INPUT))
  ANNUAL_PWC <- matrix(NA, nrow = ncell(var1.stack), ncol = length(YEAR_INPUT))
  REGIONAL_ANNUAL <- list()

  for (y in 1:length(YEAR_INPUT)){
    YOI <- YEAR_INPUT[[y]]
    # print(YOI)
    layer_names_y <- layer_names[which(years == YOI)]
    variable_list <- list()
    for (i in 1:length(formals(HS))) {
      # assign SPAM HA area filter
      assign(paste0("var", i, ".SPAM.y"), get(paste0("var", i, ".stack"))[[which(years == YOI)]] * HA_filter)
      variable_list[[i]] <- get(paste0("var", i, ".SPAM.y"))
    }
    # Use do.call to overlay the climate variable list
    HS.stack.y <- do.call(overlay, c(variable_list, fun = HS))
    names(HS.stack.y) <- layer_names_y
    HS.stack.y <- stack(HS.stack.y)
    # TODO: how to make this step faster
    PWC.stack.y <- stack(lapply(1:nlayers(HS.stack.y), function(nlay) {calc(HS.stack.y[[nlay]], LHR)})) # 19s
    names(PWC.stack.y) <- layer_names_y
    layer_dates_y <- as.Date(gsub("X", "", layer_names_y), format = "%Y.%m.%d")
    date_range_y <- range(layer_dates_y)
    dates_y <- seq.Date(date_range_y[1], date_range_y[2], by = "day")
    month_y <- format(dates_y, "%m")
    months_y <- as.integer(month_y)
    unique_months_y <- unique(as.vector(months_y))
    MONTH_HS <- list()
    MONTH_PWC <- list()
    for (m in 1:length(unique_months_y)){
      MOI <- unique_months_y[[m]]
      month_HS <- HS.stack.y[[which(months_y == MOI)]]
      month_mean_HS <- stackApply(month_HS, 1, fun = mean, na.rm = TRUE)
      MONTH_HS[[m]] <- month_mean_HS
      month_PWC <- PWC.stack.y[[which(months_y == MOI)]]
      month_mean_PWC <- stackApply(month_PWC, 1, fun = mean, na.rm = TRUE)
      MONTH_PWC[[m]] <- month_mean_PWC
    }
    month_HS_stack <- stack(MONTH_HS)
    month_PWC_stack <- stack(MONTH_PWC)

    month_HS_mtx <- raster::as.matrix(month_HS_stack)
    month_PWC_mtx <- raster::as.matrix(month_PWC_stack)

    # apply the crop calendar weight matrix, the rowSums of weight = 1,
    # so use the rowSums to calculate the annual mean

    month_HS_mtx_calendar <-  month_HS_mtx * crop_calendar
    annual_HS_mean <- apply(month_HS_mtx_calendar, 1, function(row) {
      if (all(is.na(row))) {
        return(NA)  # Return NA if the entire row is NA
      } else {
        return(sum(row, na.rm = TRUE))  # Sum while ignoring NA
      }
    })

    month_PWC_mtx_calendar <- month_PWC_mtx * crop_calendar
    annual_PWC_mean <- apply(month_PWC_mtx_calendar, 1, function(row) {
      if (all(is.na(row))) {
        return(NA)  # Return NA if the entire row is NA
      } else {
        return(sum(row, na.rm = TRUE))  # Sum while ignoring NA
      }
    })
    ANNUAL_HS[, y] <- annual_HS_mean
    ANNUAL_PWC[, y] <- annual_PWC_mean
  }
  HS.output <- cbind(Coordinates_template, ANNUAL_HS) %>% setNames(c("x", "y", paste0("HS_",YEAR_INPUT)))
  PWC.output <- cbind(Coordinates_template, ANNUAL_PWC) %>% setNames(c("x", "y", paste0("PWC_",YEAR_INPUT)))
  list(HS.output, PWC.output)
}
