## code to prepare `DATASET` dataset goes here
library(dplyr)
library(ncdf4)
library(raster)
library(tidyr)

# SECTOR CHOICE SET ----
SECTOR_ALL <- c("MAIZ_I", "RICE_I", "SOYB_I", "WHEA_I",
                "MAIZ_R", "RICE_R", "SOYB_R", "WHEA_R",
                "NONCROP")


# SECTOR MONTH WEIGHTS ----

ISIMIP_SPAM <- function(ISIMIP_crop){
  SPAM_crop <- gsub("mai_ir", "MAIZ_I", ISIMIP_crop)
  SPAM_crop <- gsub("mai_rf", "MAIZ_R", SPAM_crop)
  SPAM_crop <- gsub("ri1_ir", "RICE_I", SPAM_crop)
  SPAM_crop <- gsub("ri1_rf", "RICE_R", SPAM_crop)
  SPAM_crop <- gsub("ri2_ir", "RICE_I", SPAM_crop)
  SPAM_crop <- gsub("ri2_rf", "RICE_R", SPAM_crop)
  SPAM_crop <- gsub("ri2_ir", "RICE_I", SPAM_crop)
  SPAM_crop <- gsub("ri2_rf", "RICE_R", SPAM_crop)
  SPAM_crop <- gsub("soy_ir", "SOYB_I", SPAM_crop)
  SPAM_crop <- gsub("soy_rf", "SOYB_R", SPAM_crop)
  SPAM_crop <- gsub("swh_ir", "WHEA_I", SPAM_crop)
  SPAM_crop <- gsub("swh_rf", "WHEA_R", SPAM_crop)
  SPAM_crop <- gsub("wwh_ir", "WHEA_I", SPAM_crop)
  SPAM_crop <- gsub("wwh_rf", "WHEA_R", SPAM_crop)
  return(SPAM_crop)
}

days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

input_dir <- system.file("extdata/ggcmi_phase3_crop_calendar", package = "HELPS")

file_list <-  list.files(input_dir, pattern=".nc4", all.files=FALSE, full.names=FALSE)
N <- length(file_list); N
gsub("_ggcmi_crop_calendar_phase3_v1.01.nc4", "", file_list)



## Crop sector monthly weights ----
# TODO: expand to other crops
ggcmi_key <- c("mai", "ri", "soy", "wh")

### IRR  ----
IRR_list <- file_list[grep("ir", file_list)]; IRR_list
file_list_major4 <- IRR_list
CALENDAR_MONTH_EXTENDED <- list() # combine relevant crop calendar for an aggregated crop group
for (crop_index in 1:length(ggcmi_key)){

  file_list_key <- file_list_major4[grep(paste(ggcmi_key[crop_index], collapse = "|"), file_list_major4)]; file_list_key
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
  CALENDAR_MONTH_EXTENDED[[crop_index]] <- extended_weight
}

CALENDAR_MONTH_EXTENDED_MAJOR4_IRR <- CALENDAR_MONTH_EXTENDED

### RFD  ----

RFD_list <- file_list[grep("rf", file_list)]; RFD_list
file_list_major4 <- RFD_list
CALENDAR_MONTH_EXTENDED <- list() # combine relevant crop calendar for an aggregated crop group
for (crop_index in 1:length(ggcmi_key)){

  file_list_key <- file_list_major4[grep(paste(ggcmi_key[crop_index], collapse = "|"), file_list_major4)]; file_list_key
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
  CALENDAR_MONTH_EXTENDED[[crop_index]] <- extended_weight
}
CALENDAR_MONTH_EXTENDED_MAJOR4_RFD <- CALENDAR_MONTH_EXTENDED

###  Combine IRR&RFD ----
CALENDAR_MONTH_EXTENDED_MAJOR4 <- c(CALENDAR_MONTH_EXTENDED_MAJOR4_IRR, CALENDAR_MONTH_EXTENDED_MAJOR4_RFD)

## Non-crop sector monthly weights ----

# every month weights equally, filter out grids w/o population

# POP <- raster("../raw data/LANDSCAN/landscan-global-2019.tif")
# POP_agg <- aggregate(POP, fact=60, fun=sum, na.rm=TRUE)
# writeRaster(POP_agg, filename = "Grid_POP.tif", format = "GTiff", overwrite = TRUE)

Grid_POP <- raster(system.file("extdata/landscan_global_2019/Grid_POP.tif", package = "HELPS"))
FLAG_noncrop <- Grid_POP
values(FLAG_noncrop) <- ifelse(values(Grid_POP) > 0, 1, NA)


Grid_POP %>% as.data.frame(xy = T) %>%
  dplyr::mutate(value = ifelse(Grid_POP > 0, 1, 0)) -> df_grid_pop

df_grid_pop[paste0("Mon", 1:12)] <- 1 / 12 # equal month weight for non-crop sector -> df_grid_pop

# update the month weights by population grid filter
df_grid_pop %>%
  dplyr::mutate(across(starts_with("Mon"), ~ . * value)) %>%
  dplyr::select(Mon1:Mon12) %>% as.matrix() ->
  CALENDAR_MONTH_NONCROP

## save pkg data ----
### Combine crop & noncrop ----
SECTOR_MONTH_WEIGHT <- c(CALENDAR_MONTH_EXTENDED_MAJOR4, list(CALENDAR_MONTH_NONCROP))

# SECTOR FLAG ----

## non-crop sector ----
# based on population
flag_noncrop <- as.matrix(df_grid_pop$value)
smw_non_crop <- as.matrix(df_grid_pop %>% dplyr::select(x, y, value = Grid_POP))

## crop sector
# based on SPAM harvested area (unit: ha)

# SPAM_dir <- system.file("extdata/spam2020V1r0_global_harvested_area", package = "HELPS")
SPAM_dir <- "inst/extdata/spam2020V1r0_global_harvested_area"
HA_list = list.files(path=SPAM_dir, pattern=".tif", all.files=FALSE, full.names=TRUE); HA_list

HA_irr_list = list.files(path=SPAM_dir, pattern="_I.tif", all.files=FALSE, full.names=TRUE); HA_irr_list
HA_rfd_list = list.files(path=SPAM_dir, pattern="_R.tif", all.files=FALSE, full.names=TRUE); HA_rfd_list

# TODO: expand to other crops
key = c("MAIZ", "RICE", "WHEA", "SOYB")

## IRR: raster ----
major4_files <- HA_irr_list[grep(paste(key, collapse = "|"), HA_irr_list)]; major4_files
SPAM_HA <- list()
SPAM_HA_W <- list()
for(i in 1:length(major4_files)){
  Area <- raster(major4_files[[i]])
  Area_agg <- aggregate(Area, fact=6, fun=sum, na.rm=TRUE)
  FLAG <- Area_agg
  values(FLAG) <- ifelse(values(Area_agg) > 0, 1, NA)
  SPAM_HA[[i]] <- FLAG
  SPAM_HA_W[[i]] <- Area_agg
}
SPAM_HA_IRR <- SPAM_HA
SPAM_HA_W_IRR <- SPAM_HA_W

## RFD: raster ----
major4_files <- HA_rfd_list[grep(paste(key, collapse = "|"), HA_rfd_list)]; major4_files
SPAM_HA <- list()
SPAM_HA_W <- list()
for(i in 1:length(major4_files)){
  Area <- raster(major4_files[[i]])
  Area_agg <- aggregate(Area, fact=6, fun=sum, na.rm=TRUE)
  FLAG <- Area_agg
  values(FLAG) <- ifelse(values(Area_agg) > 0, 1, NA)
  SPAM_HA[[i]] <- FLAG
  SPAM_HA_W[[i]] <- Area_agg
}
SPAM_HA_RFD <- SPAM_HA
SPAM_HA_W_RFD <- SPAM_HA_W
## Combine crop & noncrop ----
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

# save pkg data ----

#' SECTOR_ALL
#'
#' A vector of sector available to choose from, consists of 8 crop sectors and 1 noncrop sector
#' @author DS
usethis::use_data(SECTOR_ALL, overwrite = TRUE)


#' SECTOR_MONTH_WEIGHT
#'
#' A list of prebuilt grid-level month weights data objects, consists of 8 crop sectors and 1 noncrop sector
#' The output provides grid-level monthly weights to build annual mean value
#' This weights make sure annual mean from monthly values equals to the annual mean from daily values
#' Source data is from ISIMIP3b, need to check the data release requirement
#' @author DS
#' crop-by-mgmt transform: from ISIMIP to SPAM
#' combine ri1 and ri2 to rice
#' combine spring wheat (swh) and winter wheat (wwh) to wheat
#' For noncrop sector, every month weights equally
#' non-relevant grids are assigned NA value
usethis::use_data(SECTOR_MONTH_WEIGHT, overwrite = TRUE)



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
#' A list of prebuilt rasters of harvested area or population by sector, including 8 crop sectors and 1 noncrop sector
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
