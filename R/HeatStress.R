#' HeatStress
#'
#' This function takes specified climate .nc files and a user-specifed heat stress function as arguments,
#' reshapes them, calculates the heat stress and returns as raster
#'
#' @param TempRes temporal resolution of input, "day" or "month"
#' @param SECTOR choice of sector
#' @param HS choice of heat stress function
#' @param YEAR_INPUT a vector of years of interest
#' @param ... individual .nc file name of climate variables in order
#'
#' @importFrom raster stack overlay calc stackApply as.matrix ncell nlayers
#' @importFrom terra rast
#' @importFrom dplyr %>%
#' @import assertthat
#'
#' @return A raster stack of daily grid-level heat stress level
#' @export
#'
#' @examples
#' \dontrun{
#' GD_HS <- DAY2ANN(TempRes = "day", SECTOR = "MAIZ_I", HS = WBGT1, YEAR_INPUT = 2027,
#' "hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#' "tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#' "ps_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
#' }

HeatStress <- function(TempRes, SECTOR, HS, YEAR_INPUT, ...){

  # check the input temporal resolution
  if (missing(TempRes) || TempRes == "" || !TempRes %in% c("day", "month")) {
    stop("Error: Please specify TempRes: 'day' or 'month'.")
  }

    climate_vars <- list(...)
    HS_arg_count <- length(formals(HS))
    if (length(climate_vars) != HS_arg_count) {
      stop("The number of climate variables does not match the HS function's requirements.")
    }

    # read in individual files of climate variables
    for (i in seq_along(climate_vars)) {
      assign(paste0("var", i,".stack"), terra::rast(climate_vars[[i]]))
    }

    SECTOR_INDEX <- which(SECTOR_ALL == SECTOR)
    sector_filter <- SECTOR_FLAG[[SECTOR_INDEX]]

    # -----
    # subset layers for year of interest
    layer_dates <- terra::time(var1.stack)
    YEAR <- format(layer_dates, "%Y")


    assertthat::assert_that(
      YEAR_INPUT %in% unique(YEAR),
      msg = paste0("invalid YEAR_INPUT, make sure input files include data for ", YEAR_INPUT))

    # subset layers falls into the year of interest
    year_layer_index <- which(YEAR == YEAR_INPUT)

    # check number of layers
    if (TempRes == "day") {
      assertthat::assert_that(
        length(year_layer_index) %in% c(365, 366),
        msg = paste0("For daily input, expect 365 or 366 layers, but got ", length(year_layer_index), " layers.")
      )
    } else if (TempRes == "month") {
      assertthat::assert_that(
        length(year_layer_index) == 12,
        msg = paste0("For monthly input, expect 12 layers, but got ", length(year_layer_index), " layers.")
      )
    }

    variable_list <- list()
    for (i in 1:length(formals(HS))) {
      # assign SECTOR grid filter raster
      assign(paste0("var", i, ".SECTOR.y"), var.i.SECTOR.y <-  get(paste0("var", i, ".stack"))[[year_layer_index]] * terra::rast(sector_filter))
      rm(list = paste0("var", i, ".stack"))
      layer_dates.y <- terra::time(get(paste0("var", i, ".SECTOR.y")))
      assign(paste0("stack", i, ".SECTOR.y"), raster::brick(get(paste0("var", i, ".SECTOR.y"))))
      rack <- get(paste0("stack", i, ".SECTOR.y"))
      rm(list = paste0("stack", i, ".SECTOR.y"))
      # names(rack) <- layer_dates.y
      variable_list[[i]] <- rack
    }
    HS.stack.y <- do.call(overlay, c(variable_list, fun = HS))
    HS.stack.y <- stack(HS.stack.y)
    names(HS.stack.y) <- layer_dates.y # name it so that DAY2MON function can work as expected
    # check number of layers
    if (TempRes == "day") {
      assert_that(
        nlayers(HS.stack.y) %in% c(365, 366),
        msg = paste0("Error: For daily input, expect 365 or 366 layers, but got ", nlayers(HS.stack.y), " layers.")
      )
    } else if (TempRes == "month") {
      assert_that(
        nlayers(HS.stack.y) == 12,
        msg = paste0("Error: For monthly input, expect 12 layers, but got ", nlayers(HS.stack.y), " layers.")
      )
    }
    return(HS.stack.y)
  }
