#' monthly_to_annual
#'
#' Calculate Annual Grid-Level Values from Monthly Raster Data
#' The `monthly_to_annual` function computes annual grid-level values from a raster stack of monthly data,
#' This function supports a wide range of agricultural sectors and accounts for variations in crop calendars.

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
#' annual_PWC <- monthly_to_annual(input_rack = month_PWC, SECTOR = "MAIZ_I")
#' annual_HS <- monthly_to_annual(input_rack = month_HS, SECTOR = "MAIZ_I")
#' }
monthly_to_annual <- function(input_rack, SECTOR) {
  if (SECTOR %in% c(
    "WHEA_I", "RICE_I", "MAIZ_I", "SOYB_I", "BARL_I", "MILL_I", "PMIL_I", "SORG_I", "OCER_I",
    "POTA_I", "SWPO_I", "YAMS_I", "CASS_I", "BEAN_I", "CHIC_I", "COWP_I", "PIGE_I", "LENT_I",
    "GROU_I", "SUNF_I", "RAPE_I", "SESA_I", "SUGC_I", "SUGB_I", "COTT_I",
    "WHEA_R", "RICE_R", "MAIZ_R", "SOYB_R", "BARL_R", "MILL_R", "PMIL_R", "SORG_R", "OCER_R",
    "POTA_R", "SWPO_R", "YAMS_R", "CASS_R", "BEAN_R", "CHIC_R", "COWP_R", "PIGE_R", "LENT_R",
    "GROU_R", "SUNF_R", "RAPE_R", "SESA_R", "SUGC_R", "SUGB_R", "COTT_R"
  )) {
    SECTOR_INDEX <- which(SECTOR_GGCMI == SECTOR)
    month_weight <- SECTOR_MONTH_WEIGHT[[SECTOR_INDEX]]
  } else if (SECTOR %in% c(
    "OFIB_I", "BANA_I", "PLNT_I", "CITR_I", "TROF_I", "TEMF_I", "TOMA_I", "ONIO_I", "VEGE_I",
    "ORTS_I", "OPUL_I", "CNUT_I", "OILP_I", "OOIL_I", "COFF_I", "RCOF_I", "COCO_I", "RUBB_I",
    "TEAS_I", "TOBA_I", "REST_I",
    "OFIB_R", "BANA_R", "PLNT_R", "CITR_R", "TROF_R", "TEMF_R", "TOMA_R", "ONIO_R", "VEGE_R",
    "ORTS_R", "OPUL_R", "CNUT_R", "OILP_R", "OOIL_R", "COFF_R", "RCOF_R", "COCO_R", "RUBB_R",
    "TEAS_R", "TOBA_R", "REST_R",
    "NONCROP"
  )) {
    month_weight <- extended_weight_others
  }
  input_mtx <- as.matrix(input_rack) * month_weight
  annual_mean <- apply(input_mtx, 1, function(row) if (all(is.na(row))) NA else sum(row, na.rm = TRUE))
  annual_output <- as.data.frame(input_rack, xy = T) %>% dplyr::select(x, y)
  annual_output$value <- annual_mean
  return(annual_output)
}
