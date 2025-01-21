#' grid_to_region
#'
#' Aggregate Grid-Level Annual Values to Regional-Level
#'
#' The `grid_to_region` function aggregates grid-level annual values to regional scales using spatial mapping
#' weights and rasterized regional boundaries. This allows for the computation of regional annual
#' values for a specified sector.
#'
#' @param grid_annual_value data.frame of grid-level annual value
#' @param SECTOR choice of sector
#' @param rast_boundary rasterized regional boundaries, 0.5 degree, Set to global extent
#'
#' @importFrom dplyr %>%
#' @import assertthat
#'
#' @return A sf data frame of regional annual value
#' @export
#'
#' @examples
#' \dontrun{
#' reg_annual_value <- grid_to_region(grid_annual_value = pwc.hothaps.ann, SECTOR = "MAIZ_R", rast_boundary = country_raster)
#' }
grid_to_region <- function(grid_annual_value, SECTOR, rast_boundary) {
  # obtain spatial mapping weights information based on the SECTOR choice
  SECTOR_INDEX <- which(HELPS::SECTOR_ALL == SECTOR)
  smw <- HELPS::SECTOR_SMW[[SECTOR_INDEX]]
  smw_df <- smw %>%
    as.data.frame(xy = T) %>%
    setNames(c("x", "y", "smw"))

  # transform the rasterized regional boundaries to data.frame
  rast_boundary %>% as.data.frame(xy = T) -> rast_boundary_df

  # check if (x,y) are identical between grid_annual_value, smw_df and rast_boundary_df
  assertthat::assert_that(
    identical(round(grid_annual_value$x, 2), round(smw_df$x, 2), round(rast_boundary_df$x, 2)),
    msg = paste0("longtitudes don't match between grid_annual_value and smw_df ")
  )

  assertthat::assert_that(
    identical(round(grid_annual_value$y, 2), round(smw_df$y, 2), round(rast_boundary_df$y, 2)),
    msg = paste0("latitudes don't match between grid_annual_value and smw_df ")
  )

  grid_annual_value$smw <- smw_df$smw
  grid_annual_value$region_id <- rast_boundary_df[, 3]

  grid_annual_value %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarise(value = weighted.mean(value, smw, na.rm = T)) ->
  reg_annual_value
  return(reg_annual_value)
}
