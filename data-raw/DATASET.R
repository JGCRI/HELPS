## code to prepare `DATASET` dataset goes here
library(dplyr)
library(ncdf4)
library(raster)
library(tidyr)



#' CALENDAR_MONTH_EXTENDED_MAJOR4_IRR
#'
#' A list of prebuilt grid-level crop calendar data objects.
#' The output provides grid-level monthly weights to build annual mean value
#' This weights make sure annual mean from monthly values equals to the annual mean from daily values
#' Source data is from ISIMIP3b, need to check the data release requirement
#' @author DS
# crop-by-mgmt transform: from ISIMIP to SPAM
# combine ri1 and ri2 to rice
# combine spring wheat (swh) and winter wheat (wwh) to wheat
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

## IRR & RFD  ----
IRR_list <- file_list[grep("ir", file_list)]; IRR_list

ggcmi_key <- c("mai", "ri", "soy", "wh")

CALENDAR_MONTH_EXTENDED <- list()
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
  # form the extended crop calendar information by combining crop calendar across subcrops
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

usethis::use_data(CALENDAR_MONTH_EXTENDED_MAJOR4_IRR, overwrite = TRUE)
