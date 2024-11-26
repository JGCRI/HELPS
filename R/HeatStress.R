#' HeatStress
#'
#' @param SECTOR choice of sector
#' @param HS choice of heat stress function
#' @param YEAR_INPUT a vector of years of interest
#' @param ... individual .nc file name of climate variables in order
#'
#' @importFrom raster stack overlay calc stackApply as.matrix ncell nlayers
#' @importFrom dplyr %>%
#'
#' @return A raster stack of daily grid-level heat stress level
#' @export
#'
#' @examples
#' \dontrun{
#' GD_HS <- DAY2ANN(SECTOR = "MAIZ_I", HS = WBGT1, YEAR_INPUT = 2027, TempRes = "Day",
#' "hurs_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#' "tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4",
#' "ps_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_20210101-20301231.nc4")
#' }

HeatStress <- function(SECTOR, HS, YEAR_INPUT, ...){
    # daily inputs
    climate_vars <- list(...)
    HS_arg_count <- length(formals(HS))
    if (length(climate_vars) != HS_arg_count) {
      stop("The number of climate variables does not match the HS function's requirements.")
    }

    # read in individual files of climate variables
    for (i in seq_along(climate_vars)) {
      assign(paste0("var", i,".stack"), raster::stack(climate_vars[[i]]))
    }

    SECTOR_INDEX <- which(SECTOR_ALL == SECTOR)
    sector_filter <- SECTOR_FLAG[[SECTOR_INDEX]]

    # read in individual files of climate variables
    for (i in seq_along(climate_vars)) {
      assign(paste0("var", i,".stack"), raster::stack(climate_vars[[i]]))
    }

    # subset layers for year of interest
    layer_names <- names(var1.stack)
    layer_dates <- as.Date(gsub("X", "", layer_names), format = "%Y.%m.%d")
    YEAR <- format(layer_dates, "%Y")

    if (YEAR_INPUT %in% unique(YEAR)) { # include all available years in the original file
      YEAR_INPUT <- YEAR_INPUT
      # If YEAR_INPUT is a subset of unique_years, proceed with the given YEAR_INPUT
    } else {
      # If X is not a subset of Y, stop with an error
      stop("Error: invalid YEAR_INPUT, make sure input files include data for all years specified in YEAR_INPUT")
    }

    # subset layers falls into the year of interest
    year_layer_index <- which(YEAR == YEAR_INPUT) # length of 365 or 366
    year_layer_names <- layer_names[year_layer_index]
    variable_list <- list()

    for (i in 1:length(formals(HS))) {
      # assign SECTOR grid filter raster
      assign(paste0("var", i, ".SECTOR.y"), get(paste0("var", i, ".stack"))[[year_layer_index]] * sector_filter)
      rm(list = paste0("var", i, ".stack"))
      variable_list[[i]] <- get(paste0("var", i, ".SECTOR.y"))
      rm(list = paste0("var", i, ".SECTOR.y"))
    }
    HS.stack.y <- do.call(overlay, c(variable_list, fun = HS))
    rm(variable_list)
    HS.stack.y <- stack(HS.stack.y)
    names(HS.stack.y) <- year_layer_names
    return(HS.stack.y)
  }
