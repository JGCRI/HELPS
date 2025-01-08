## code to prepare `DATASET` dataset goes here
library(dplyr)
library(ncdf4)
library(raster)
library(tidyr)
library(sf)

Grid_POP <- raster(system.file("extdata/landscan_global_2019/Grid_POP.tif", package = "HELPS"))
FLAG_noncrop <- Grid_POP
values(FLAG_noncrop) <- ifelse(values(Grid_POP) > 0, 1, NA)
Grid_POP %>% as.data.frame(xy = T) %>%
  dplyr::mutate(value = ifelse(Grid_POP > 0, 1, 0)) -> df_grid_pop

# PACKAGE DATA ----

# SECTOR_ALL <- c("MAIZ_I", "RICE_I", "SOYB_I", "WHEA_I",
#                 "MAIZ_R", "RICE_R", "SOYB_R", "WHEA_R",
#                 "NONCROP")

## SECTOR_ALL ----
SECTOR_ALL <- c("WHEA_I", "RICE_I", "MAIZ_I", "SOYB_I", "BARL_I", "MILL_I", "PMIL_I", "SORG_I", "OCER_I",
                "POTA_I", "SWPO_I", "YAMS_I", "CASS_I", "BEAN_I", "CHIC_I", "COWP_I", "PIGE_I", "LENT_I",
                "GROU_I", "SUNF_I", "RAPE_I", "SESA_I", "SUGC_I", "SUGB_I", "COTT_I", "OFIB_I", "BANA_I",
                "PLNT_I", "CITR_I", "TROF_I", "TEMF_I", "TOMA_I", "ONIO_I", "VEGE_I", "ORTS_I", "OPUL_I",
                "CNUT_I", "OILP_I", "OOIL_I", "COFF_I", "RCOF_I", "COCO_I", "RUBB_I", "TEAS_I", "TOBA_I",
                "REST_I",
                "WHEA_R", "RICE_R", "MAIZ_R", "SOYB_R", "BARL_R", "MILL_R", "PMIL_R", "SORG_R", "OCER_R",
                "POTA_R", "SWPO_R", "YAMS_R", "CASS_R", "BEAN_R", "CHIC_R", "COWP_R", "PIGE_R", "LENT_R",
                "GROU_R", "SUNF_R", "RAPE_R", "SESA_R", "SUGC_R", "SUGB_R", "COTT_R", "OFIB_R", "BANA_R",
                "PLNT_R", "CITR_R", "TROF_R", "TEMF_R", "TOMA_R", "ONIO_R", "VEGE_R", "ORTS_R", "OPUL_R",
                "CNUT_R", "OILP_R", "OOIL_R", "COFF_R", "RCOF_R", "COCO_R", "RUBB_R", "TEAS_R", "TOBA_R",
                "REST_R",
                "NONCROP")

days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
regular_month_weight <- days_in_month / sum(days_in_month)

# PACKAGE DATA ----
## ISIMIP: crop calendar ----

input_dir <- system.file("extdata/ggcmi_phase3_crop_calendar", package = "HELPS")

file_list <-  list.files(input_dir, pattern=".nc4", all.files=FALSE, full.names=FALSE)
N <- length(file_list); N
# ggcmi_list <- gsub("_ggcmi_crop_calendar_phase3_v1.01.nc4", "", file_list)
# # remove irrigation practice, and only keep crop name: _.*
# # note that ri1 and ri2 are both rice, so remove number: \\d+
# # get the unique list of ggcmi crops
# ggcmi_list <- gsub("(_.*|\\d+)", "", ggcmi_list)
# # winter wheat and spring wheat are both wheat
# ggcmi_list <- gsub("swh", "wh", ggcmi_list)
# ggcmi_list <- gsub("wwh", "wh", ggcmi_list)
# ggcmi_list <- unique(ggcmi_list); ggcmi_list

ggcmi_key <- c("wh", "ri", "mai", "soy", "bar", "mil", "mil", "sor",
               "rye", "pot", "cas", "cas", "cas", "bea", "pea", "pea",
               "pea", "pea", "nut", "sun", "rap", "sun", "sgc", "sgb", "cot")


### IRR  ----
IRR_list <- file_list[grep("ir", file_list)]; IRR_list
file_list_crop <- IRR_list
CALENDAR_MONTH_EXTENDED <- list() # combine relevant crop calendar for an aggregated crop group
for (crop_index in 1:length(ggcmi_key)){

  file_list_key <- file_list_crop[grep(paste(ggcmi_key[crop_index], collapse = "|"), file_list_crop)]; file_list_key
  NN <- length(file_list_key); NN

  CALENDAR_MONTH_ELEMENT <- list()

  for (j in 1:NN){
    ggcmi_nc <- file_list_key[[j]]
    crop <- gsub("_ggcmi_crop_calendar_phase3_v1.01.nc4", "", ggcmi_nc); crop

    plant <- raster(paste0(input_dir,"/",ggcmi_nc), varname = "planting_day")
    maturity <- raster(paste0(input_dir,"/",ggcmi_nc), varname = "maturity_day")

    plant.df = raster::as.data.frame(plant, xy = TRUE)
    names(plant.df) <- c("x", "y", "Planting")
    maturity.df = raster::as.data.frame(maturity, xy = TRUE)
    names(maturity.df) <- c("x", "y", "Maturity")

    # make sure the x, and y order remains the same
    # so tha tin later stage can be directly combined with raster.df
    df <- plant.df
    df$Maturity <- maturity.df$Maturity
    rm(plant.df, maturity.df)
    df %>% mutate(
      Crop = crop,
      # planting month
      Planting1 = pmax(floor(Planting / 30), 1),
      # maturity month, could > 12 due to crop calendar cross calendar year
      Maturity1 = ceiling(ifelse(Planting < Maturity, Maturity / 30, (Maturity + 365) / 30)),
      # use the same year data if crop calendar cross calendar year
      Maturity2 = ifelse(Maturity1 > 12, Maturity1 - 12, Maturity1)) ->
      df

    for (i in 1:12) {
      df[[paste0("Mon", i)]] <- as.integer(i >= df$Planting1 & i <= df$Maturity1 |
                                             (df$Maturity2 < df$Maturity1 & (i <= df$Maturity2)) )
    }

    # to align the annual mean derived from monthly means and the annual mean derived from daily values,
    # replace 1 with the monthly weight, where the weight
    # = # of month days / sum(# of days across crop calendar months)

    mtx <- df %>% dplyr::select(Mon1:Mon12) %>% as.matrix()
    # apply SPAM grid level data

    # replace 0 with NA, as those months are outside of crop calendar
    mtx[mtx == 0] <- NA

    # Calculate the updated matrix
    weight_mtx <- mtx
    CALENDAR_MONTH_ELEMENT[[j]] <- mtx
  }
  # build the extended crop calendar information by combining crop calendar across subcrops
  # element-wise "OR" function
  extended_mtx <- Reduce(`|`, CALENDAR_MONTH_ELEMENT) * 1
  extended_weight <- extended_mtx
  # Update each row separately
  for (i in 1:nrow(extended_mtx)) {
    # Total days for crop calendar months
    total_days <- sum(days_in_month[extended_mtx[i, ] == 1], na.rm = TRUE)
    if (total_days > 0) {
      extended_weight[i, ] <- days_in_month[extended_mtx[i, ] == 1] / total_days
    }
  }
  # extended_weight_int <- matrix(as.integer(extended_weight * 100), nrow = nrow(extended_weight), ncol = ncol(extended_weight))
  CALENDAR_MONTH_EXTENDED[[crop_index]] <- extended_weight
}

CALENDAR_MONTH_EXTENDED_CROP_IRR <- CALENDAR_MONTH_EXTENDED
rm(CALENDAR_MONTH_EXTENDED)

### RFD  ----

RFD_list <- file_list[grep("rf", file_list)]; RFD_list
file_list_crop <- RFD_list
CALENDAR_MONTH_EXTENDED <- list() # combine relevant crop calendar for an aggregated crop group
for (crop_index in 1:length(ggcmi_key)){

  file_list_key <- file_list_crop[grep(paste(ggcmi_key[crop_index], collapse = "|"), file_list_crop)]; file_list_key
  NN <- length(file_list_key); NN

  CALENDAR_MONTH_ELEMENT <- list()

  for (j in 1:NN){
    ggcmi_nc <- file_list_key[[j]]
    crop <- gsub("_ggcmi_crop_calendar_phase3_v1.01.nc4", "", ggcmi_nc); crop

    plant <- raster(paste0(input_dir,"/",ggcmi_nc), varname = "planting_day")
    maturity <- raster(paste0(input_dir,"/",ggcmi_nc), varname = "maturity_day")

    plant.df = raster::as.data.frame(plant, xy = TRUE)
    names(plant.df) <- c("x", "y", "Planting")
    maturity.df = raster::as.data.frame(maturity, xy = TRUE)
    names(maturity.df) <- c("x", "y", "Maturity")

    # make sure the x, and y order remains the same
    # so tha tin later stage can be directly combined with raster.df
    df <- plant.df
    df$Maturity <- maturity.df$Maturity
    rm(plant.df, maturity.df)
    df %>% mutate(
      Crop = crop,
      # planting month
      Planting1 = pmax(floor(Planting / 30), 1),
      # maturity month, could > 12 due to crop calendar cross calendar year
      Maturity1 = ceiling(ifelse(Planting < Maturity, Maturity / 30, (Maturity + 365) / 30)),
      # use the same year data if crop calendar cross calendar year
      Maturity2 = ifelse(Maturity1 > 12, Maturity1 - 12, Maturity1)) ->
      df

    for (i in 1:12) {
      df[[paste0("Mon", i)]] <- as.integer(i >= df$Planting1 & i <= df$Maturity1 |
                                             (df$Maturity2 < df$Maturity1 & (i <= df$Maturity2)) )
    }

    # to align the annual mean derived from monthly means and the annual mean derived from daily values,
    # replace 1 with the monthly weight, where the weight
    # = # of month days / sum(# of days across crop calendar months)

    mtx <- df %>% dplyr::select(Mon1:Mon12) %>% as.matrix()
    # apply SPAM grid level data

    # replace 0 with NA, as those months are outside of crop calendar
    mtx[mtx == 0] <- NA

    # Calculate the updated matrix
    weight_mtx <- mtx
    CALENDAR_MONTH_ELEMENT[[j]] <- mtx
  }
  # build the extended crop calendar information by combining crop calendar across subcrops
  # element-wise "OR" function
  extended_mtx <- Reduce(`|`, CALENDAR_MONTH_ELEMENT) * 1
  extended_weight <- extended_mtx
  # Update each row separately
  for (i in 1:nrow(extended_mtx)) {
    # Total days for crop calendar months
    total_days <- sum(days_in_month[extended_mtx[i, ] == 1], na.rm = TRUE)
    if (total_days > 0) {
      extended_weight[i, ] <- days_in_month[extended_mtx[i, ] == 1] / total_days
    }
  }
  # extended_weight_int <- matrix(as.integer(extended_weight * 100), nrow = nrow(extended_weight), ncol = ncol(extended_weight))
  CALENDAR_MONTH_EXTENDED[[crop_index]] <- extended_weight
}
CALENDAR_MONTH_EXTENDED_CROP_RFD <- CALENDAR_MONTH_EXTENDED
rm(CALENDAR_MONTH_EXTENDED)

# PACKAGE DATA ----
## SECTOR_MONTH_WEIGHT ----

SECTOR_MONTH_WEIGHT <- c(CALENDAR_MONTH_EXTENDED_CROP_IRR,
                         CALENDAR_MONTH_EXTENDED_CROP_RFD)
rm(CALENDAR_MONTH_EXTENDED_CROP_IRR, CALENDAR_MONTH_EXTENDED_CROP_RFD)


#### for crops w/o ggcmi crop calendar, use regular_month_weight
# grids w/o harvested area weight = NA

extended_weight_others <- matrix(rep(regular_month_weight, nrow(extended_weight)), nrow = nrow(extended_weight), byrow = TRUE)
colnames(extended_weight_others) <- colnames(extended_weight)

# PACKAGE DATA ----
## SPAM: harvestes area ----

SPAM_dir <- "inst/extdata/spam2020V1r0_global_harvested_area"
HA_list <-  list.files(path=SPAM_dir, pattern=".tif", all.files=FALSE, full.names=TRUE); HA_list

HA_irr_list <-  list.files(path=SPAM_dir, pattern="_I.tif", all.files=FALSE, full.names=TRUE); HA_irr_list
HA_rfd_list <-  list.files(path=SPAM_dir, pattern="_R.tif", all.files=FALSE, full.names=TRUE); HA_rfd_list


key <- c("WHEA", "RICE", "MAIZ", "SOYB", "BARL", "MILL", "PMIL", "SORG", "OCER", "POTA", "SWPO", "YAMS", "CASS",
         "BEAN", "CHIC", "COWP", "PIGE", "LENT", "GROU", "SUNF", "RAPE", "SESA", "SUGC", "SUGB", "COTT",
         "OFIB", "BANA", "PLNT", "CITR", "TROF", "TEMF", "TOMA", "ONIO", "VEGE", "ORTS", "OPUL",
         "CNUT", "OILP", "OOIL", "COFF", "RCOF", "COCO", "RUBB", "TEAS", "TOBA", "REST")


### IRR: raster ----
# crop_files <- HA_irr_list[grep(paste(key, collapse = "|"), HA_irr_list)]; crop_files
SPAM_HA <- list()
SPAM_HA_W <- list()
for(i in 1:length(HA_irr_list)){
  # Area <- raster(crop_files[[i]])
  Area <- raster(HA_irr_list[grepl(key[i], HA_irr_list)])
  Area_agg <- aggregate(Area, fact=6, fun=sum, na.rm=TRUE)
  FLAG <- Area_agg
  values(FLAG) <- ifelse(values(Area_agg) > 0, 1, NA)
  SPAM_HA[[i]] <- FLAG
  SPAM_HA_W[[i]] <- Area_agg
}
SPAM_HA_IRR <- SPAM_HA
SPAM_HA_W_IRR <- SPAM_HA_W
rm(SPAM_HA, SPAM_HA_W)
rm(FLAG, Area_agg)

### RFD: raster ----
# crop_files <- HA_rfd_list[grep(paste(key, collapse = "|"), HA_rfd_list)]; crop_files
SPAM_HA <- list()
SPAM_HA_W <- list()
for(i in 1:length(HA_rfd_list)){
  # Area <- raster(crop_files[[i]])
  Area <- raster(HA_rfd_list[grepl(key[i], HA_rfd_list)])
  Area_agg <- aggregate(Area, fact=6, fun=sum, na.rm=TRUE)
  FLAG <- Area_agg
  values(FLAG) <- ifelse(values(Area_agg) > 0, 1, NA)
  SPAM_HA[[i]] <- FLAG
  SPAM_HA_W[[i]] <- Area_agg
}
SPAM_HA_RFD <- SPAM_HA
SPAM_HA_W_RFD <- SPAM_HA_W
rm(SPAM_HA, SPAM_HA_W)
rm(FLAG, Area_agg)



# # monthly weights for other crops
# simply use regular_month_weight with the global extend, the SPAM HA filter is applied in the HS and PWC process
# begin <- which(key == "OFIB"); begin
# end <- which(key == "REST"); end
#
# for (i in begin:end){
#   flag <- as.data.frame(SPAM_HA_IRR[[i]], xy = T) %>% setNames(c("x", "y", "flag"))
#   extended_weight_others %>% as.data.frame() %>%
#     mutate(across(starts_with("Mon"), ~ .x * flag$flag)) %>% as.matrix() -> extended_weight_irr_others
#   CALENDAR_MONTH_EXTENDED_CROP_IRR[[i]] <- extended_weight_irr_others # append other crops to the main list
# }
#
# for (i in begin:end){
#   flag <- as.data.frame(SPAM_HA_RFD[[i]], xy = T) %>% setNames(c("x", "y", "flag"))
#   extended_weight_others %>% as.data.frame() %>%
#     mutate(across(starts_with("Mon"), ~ .x * flag$flag)) %>% as.matrix() -> extended_weight_rfd_others
#   CALENDAR_MONTH_EXTENDED_CROP_RFD[[i]] <- extended_weight_rfd_others # append other crops to the main list
# }


# equal weights of each month
df_grid_pop[paste0("Mon", 1:12)] <- 1 / 12 # equal month weight for non-crop sector -> df_grid_pop
# monthly weights = days of a month / 365
# recognize the discrepancy in leap years
df_grid_pop[, grep("^Mon\\d+", names(df_grid_pop))] <- as.data.frame(t(replicate(nrow(df_grid_pop), regular_month_weight)))

# update the month weights by population grid filter
df_grid_pop %>%
  dplyr::mutate(across(starts_with("Mon"), ~ . * value)) %>%
  dplyr::select(Mon1:Mon12) %>% as.matrix() ->
  CALENDAR_MONTH_NONCROP

## SECTOR_GGCMI ----
SECTOR_GGCMI <- c("WHEA_I", "RICE_I", "MAIZ_I", "SOYB_I", "BARL_I", "MILL_I", "PMIL_I", "SORG_I", "OCER_I",
                  "POTA_I", "SWPO_I", "YAMS_I", "CASS_I", "BEAN_I", "CHIC_I", "COWP_I", "PIGE_I", "LENT_I",
                  "GROU_I", "SUNF_I", "RAPE_I", "SESA_I", "SUGC_I", "SUGB_I", "COTT_I",
                  "WHEA_R", "RICE_R", "MAIZ_R", "SOYB_R", "BARL_R", "MILL_R", "PMIL_R", "SORG_R", "OCER_R",
                  "POTA_R", "SWPO_R", "YAMS_R", "CASS_R", "BEAN_R", "CHIC_R", "COWP_R", "PIGE_R", "LENT_R",
                  "GROU_R", "SUNF_R", "RAPE_R", "SESA_R", "SUGC_R", "SUGB_R", "COTT_R")

## SECTOR FLAG ----

## non-crop sector ----
# based on population
flag_noncrop <- as.matrix(df_grid_pop$value)
smw_non_crop <- as.matrix(df_grid_pop %>% dplyr::select(x, y, value = Grid_POP))

SECTOR_FLAG <- c(SPAM_HA_IRR, SPAM_HA_RFD, list(FLAG_noncrop))
SECTOR_SMW <- c(SPAM_HA_W_IRR, SPAM_HA_W_RFD, list(Grid_POP))

# REGIONAL BOUNDARIES ----
## country ----
GCAM_country <- st_read("inst/extdata/maps/country_boundaries_moirai_combined_3p1_0p5arcmin.shp")
names(GCAM_country) <- c("key" , "ctry_nm",  "region_id",  "geometry")
# create an empty raster with global extend
r.mask <- raster(resolution = 0.5,
                 xmn = -180, xmx = 180,  # Set to global extent for example (-180 to 180 for longitude)
                 ymn = -90,  ymx = 90)
# assign spatial group to each grid
country_raster <- rasterize(GCAM_country, r.mask, field = "region_id")
names(country_raster) <- "region_id"


## GCAM water basin ----
reg_WB <- st_read("inst/extdata/maps/reg_glu_boundaries_moirai_landcells_3p1_0p5arcmin.shp")
names(reg_WB) <- c("key","glu_nm",   "reg_nm",   "region_id",   "reg_id",   "geometry")
# create an empty raster with global extend
r.mask <- raster(resolution = 0.5,
                 xmn = -180, xmx = 180,  # Set to global extent for example (-180 to 180 for longitude)
                 ymn = -90,  ymx = 90)
# assign spatial group to each grid
reg_WB_raster <- rasterize(reg_WB, r.mask, field = "region_id")
names(reg_WB_raster) <- "region_id"

# SAVE PACKAGE DATA ----

#' SECTOR_ALL
#'
#' A vector of sector available to choose from, consists of
#' 46 irrgated crop sectors, 46 rain-fed crop sectors and 1 noncrop sector
#' @author DS
usethis::use_data(SECTOR_ALL, overwrite = TRUE)


#' SECTOR_GGCMI
#'
#' A vector of sector with crop calendar information to choose from, consists of
#' 25 irrgated SPAM crop sectors, 25 rain-fed SPAM crop sectors
#' @author DS
usethis::use_data(SECTOR_GGCMI, overwrite = TRUE)


#' SECTOR_MONTH_WEIGHT
#'
#' A list of prebuilt grid-level month weights data objects, consists of
#' 46 irrgated crop sectors, 46 rain-fed crop sectors and 1 noncrop sector
#' The output provides grid-level monthly weights to aggregate monthly values to annual mean value
#' This weights make sure annual mean from monthly values equals to the annual mean from daily values
#' Source data is from https://zenodo.org/records/5062513, need to check the data release requirement
#' @author DS
#' crop-by-mgmt transform: from ISIMIP to SPAM
#' combine ri1 and ri2 to rice
#' combine spring wheat (swh) and winter wheat (wwh) to wheat
#' define a regular_month_weight, which equal to number of days in a month days divided by 365
#' for SPAM crops that does not have ggcmi crop calendar, use regular_month_weight, grids w/o SPAM harvested area has NA weights
#' For noncrop sector, regular_month_weight, grids w/o population has NA weights
#' non-relevant grids are assigned NA value
usethis::use_data(SECTOR_MONTH_WEIGHT, overwrite = TRUE)



#' extended_weight_others
#'
#' A prebuilt grid-level month weights data object, for crop sectors without ggcmi crop calendar information
#' The output provides grid-level monthly weights to aggregate monthly values to annual mean value
#' This weights make sure annual mean from monthly values equals to the annual mean from daily values
#' Source data is from https://zenodo.org/records/5062513, need to check the data release requirement
#' @author DS
#' with global extend
usethis::use_data(extended_weight_others, overwrite = TRUE)



#' SECTOR_FLAG
#'
#' A list of prebuilt grid-level filter raster data objects
#' The output provides grid-level binary filter this data to filter relevant grids in the very first step
#' This data is used to improve the data processing efficiency
#' For crop sector the filter is based on SPAM harvested area
#' For noncrop sector the filter is based on landscan population data
#' @author DS
#' non-relevant grids are assigned NA value
usethis::use_data(SECTOR_FLAG, overwrite = TRUE)


#' SECTOR_SMW
#'
#' A list of prebuilt rasters of harvested area or population by sector, including
#' 46 irrgated crop sectors, 46 rain-fed crop sectors and 1 noncrop sector
#' Harvested area is in 2020: SAPM
#' Population data is in 2019: landscan
#' harmonized to 0.5 degree
#' @author DS
usethis::use_data(SECTOR_SMW, overwrite = TRUE)


#' reg_WB_raster
#'
#' A prebuilt raster of GCAM water basins
#' grids within a water basin share the same region_id
#' @author DS
usethis::use_data(reg_WB_raster, overwrite = TRUE)

#' country_raster
#'
#' A prebuilt raster of countries
#' grids within a country share the same region_id
#' @author DS
usethis::use_data(country_raster, overwrite = TRUE)

#' GCAM_country
#'
#' A prebuilt sf data.frame of country polygons
#' @author DS
usethis::use_data(GCAM_country, overwrite = TRUE)

#' reg_WB
#'
#' A prebuilt sf data.frame of country polygons
#' @author DS
usethis::use_data(reg_WB, overwrite = TRUE)
