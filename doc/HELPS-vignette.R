## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(HELPS)
library(ncdf4)
library(raster)
library(dplyr)
library(ggplot2)
library(knitr)
library(sf)

## ----Step0, warning=FALSE, eval = FALSE---------------------------------------
# get_example_data()

## ----Step1.1, warning=FALSE---------------------------------------------------
wbgt.sun.day <- HeatStress(TempRes = "day", SECTOR = "MAIZ_R", HS = WBGT_sun, YEAR_INPUT = 2024,
                    "../HELPS_Example_Data/hurs_example_day.nc",
                    "../HELPS_Example_Data/tas_example_day.nc",
                    "../HELPS_Example_Data/ps_example_day.nc")
wbgt.sun.day

## ----Step1.1.1, warning=FALSE, fig.width=10, fig.height=6---------------------
wbgt.day.plot <- wbgt.sun.day[[183]] %>% as.data.frame(xy = T)

format_longitude <- function(lon) {
  sapply(lon, function(x) {
    if (x > 0) paste0(x, "°E")
    else if (x < 0) paste0(abs(x), "°W")
    else paste0(x, "°") # Handle 0° specifically
  })
}

format_latitude <- function(lat) {
  sapply(lat, function(x) {
    if (x > 0) paste0(x, "°N")
    else if (x < 0) paste0(abs(x), "°S")
    else paste0(x, "°") # Handle 0° specifically
  })
}
wbgt.day.plot %>%
  setNames(c("x", "y", "value")) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 25,
                       na.value = "white")+
  labs(x = "longitude", y = "latitude", fill = "WBGT",
       title = "Heat stress for labor in rain-fed maize sector: 2024-07-01") +

  scale_x_continuous(
    breaks = seq(-180, 180, by = 60), # Longitude breaks
    labels = format_longitude         # Apply custom labels
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30),    # Latitude breaks
    labels = format_latitude           # Apply custom latitude labels
  ) +
  theme_bw()

## ----Step1.2, warning=FALSE---------------------------------------------------
pwc.hothaps.day <- PWC(WBGT = wbgt.sun.day,  LHR = LHR_Hothaps, workload = "high")
pwc.hothaps.day

## ----Step1.2.1, warning=FALSE, fig.width=10, fig.height=6---------------------
pwc.day.plot <- pwc.hothaps.day[[183]] %>% as.data.frame(xy = T)

format_longitude <- function(lon) {
  sapply(lon, function(x) {
    if (x > 0) paste0(x, "°E")
    else if (x < 0) paste0(abs(x), "°W")
    else paste0(x, "°") # Handle 0° specifically
  })
}

format_latitude <- function(lat) {
  sapply(lat, function(x) {
    if (x > 0) paste0(x, "°N")
    else if (x < 0) paste0(abs(x), "°S")
    else paste0(x, "°") # Handle 0° specifically
  })
}
pwc.day.plot %>%
  setNames(c("x", "y", "value")) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = 100*(1-value))) +
  scale_fill_gradient(low = "grey85", high = "red",
                      na.value = "white")+
  labs(x = "longitude", y = "latitude", fill = "PWC \nloss(%)",
       title = "Heat-induced PWC loss for labor in rain-fed maize sector: 2024-07-01") +

  scale_x_continuous(
    breaks = seq(-180, 180, by = 60), # Longitude breaks
    labels = format_longitude         # Apply custom labels
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30),    # Latitude breaks
    labels = format_latitude           # Apply custom latitude labels
  ) +
  theme_bw()

## ----Step1.3, warning=FALSE---------------------------------------------------
wbgt.sun.mon <- DAY2MON(input_rack = wbgt.sun.day)
# rm(wbgt.sun.day)
pwc.hothaps.mon <- DAY2MON(input_rack = pwc.hothaps.day)
# rm(pwc.hothaps.day)
wbgt.sun.mon
pwc.hothaps.mon


## ----Step1.3.1, warning=FALSE, fig.width=10, fig.height=6---------------------
pwc.mon.plot <- pwc.hothaps.mon[[7]] %>% as.data.frame(xy = T)

format_longitude <- function(lon) {
  sapply(lon, function(x) {
    if (x > 0) paste0(x, "°E")
    else if (x < 0) paste0(abs(x), "°W")
    else paste0(x, "°") # Handle 0° specifically
  })
}

format_latitude <- function(lat) {
  sapply(lat, function(x) {
    if (x > 0) paste0(x, "°N")
    else if (x < 0) paste0(abs(x), "°S")
    else paste0(x, "°") # Handle 0° specifically
  })
}
pwc.mon.plot %>%
  setNames(c("x", "y", "value")) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = 100*(1-value))) +
  scale_fill_gradient(low = "grey85", high = "red",
                      na.value = "white")+
  labs(x = "longitude", y = "latitude", fill = "PWC \nloss(%)",
       title = "Monthly heat-induced PWC loss for labor in rain-fed maize sector: 2024-07") +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60), # Longitude breaks
    labels = format_longitude         # Apply custom labels
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30),    # Latitude breaks
    labels = format_latitude           # Apply custom latitude labels
  ) +
  theme_bw()

## ----Step1.4, warning=FALSE---------------------------------------------------
wbgt.sun.ann <- MON2ANN(input_rack = wbgt.sun.mon, SECTOR = "MAIZ_R")
pwc.hothaps.ann <- MON2ANN(input_rack = pwc.hothaps.mon, SECTOR = "MAIZ_R")

summary(wbgt.sun.ann)
summary(pwc.hothaps.ann)

## ----Step1.4.1, warning=FALSE, fig.width=10, fig.height=6---------------------
format_longitude <- function(lon) {
  sapply(lon, function(x) {
    if (x > 0) paste0(x, "°E")
    else if (x < 0) paste0(abs(x), "°W")
    else paste0(x, "°") # Handle 0° specifically
  })
}

format_latitude <- function(lat) {
  sapply(lat, function(x) {
    if (x > 0) paste0(x, "°N")
    else if (x < 0) paste0(abs(x), "°S")
    else paste0(x, "°") # Handle 0° specifically
  })
}
pwc.hothaps.ann %>%
  setNames(c("x", "y", "value")) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = 100*(1-value))) +
  scale_fill_gradient(low = "grey85", high = "red",
                      na.value = "white")+
  labs(x = "longitude", y = "latitude", fill = "PWC \nloss(%)",
       title = "Annual heat-induced PWC loss for labor in rain-fed maize sector: 2024") +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60), # Longitude breaks
    labels = format_longitude         # Apply custom labels
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30),    # Latitude breaks
    labels = format_latitude           # Apply custom latitude labels
  ) +
  theme_bw()

## ----Step2.1, warning=FALSE---------------------------------------------------
esi.mon <- HeatStress(TempRes = "month", SECTOR = "MAIZ_R", HS = WBGT_ESI, YEAR_INPUT = 2024,
                    "../HELPS_Example_Data/hurs_example_month.nc",
                    "../HELPS_Example_Data/tas_example_month.nc",
                    "../HELPS_Example_Data/rsds_example_month.nc")
esi.mon

## ----Step2.2.1, warning=FALSE-------------------------------------------------
start_t = Sys.time()
pwc.mon.foster <- PWC(WBGT = esi.mon,  LHR = LHR_Foster, workload = "high")
end_t = Sys.time()
end_t - start_t
pwc.mon.foster

## ----Step2.2.2, warning=FALSE-------------------------------------------------
start_t = Sys.time()
pwc.mon.hothaps <- PWC(WBGT = esi.mon,  LHR = LHR_Hothaps, workload = "high")
end_t = Sys.time()
end_t - start_t
pwc.mon.hothaps

## ----Step2.2.3, warning=FALSE-------------------------------------------------
start_t = Sys.time()
pwc.mon.niosh <- PWC(WBGT = esi.mon,  LHR = LHR_NIOSH, workload = "high")
end_t = Sys.time()
end_t - start_t
pwc.mon.niosh

## ----Step2.2.4, warning=FALSE-------------------------------------------------
start_t = Sys.time()
pwc.mon.iso <- PWC(WBGT = esi.mon,  LHR = LHR_ISO, workload = "high")
end_t = Sys.time()
end_t - start_t
pwc.mon.iso

## ----Step2.3, warning=FALSE---------------------------------------------------
pwc.hothaps.ann2 <- MON2ANN(input_rack = pwc.mon.hothaps, SECTOR = "MAIZ_R")
summary(pwc.hothaps.ann2)

## ----warning=FALSE, echo = FALSE, include = FALSE-----------------------------

GCAM_country <- st_read("../inst/extdata/maps/country_boundaries_moirai_combined_3p1_0p5arcmin.shp")
GCAM_country.df <- as.data.frame(GCAM_country)

reg_WB <- st_read("../inst/extdata/maps/reg_glu_boundaries_moirai_landcells_3p1_0p5arcmin.shp")
reg_WB.df <- as.data.frame(reg_WB)

r.mask <- raster(resolution = 0.5,
                 xmn = -180, xmx = 180,  # Set to global extent for example (-180 to 180 for longitude)
                 ymn = -90,  ymx = 90)
# assign spatial group to each grid
reg_WB_raster <- rasterize(reg_WB, r.mask, field = "glu_id")
names(reg_WB_raster) <- "glu_id"

# create an empty raster with extend
r.mask <- raster(resolution = 0.5,
                 xmn = -180, xmx = 180,  # Set to global extent for example (-180 to 180 for longitude)
                 ymn = -90,  ymx = 90)
# assign spatial group to each grid
country_raster <- rasterize(GCAM_country, r.mask, field = "ctry_id")
names(country_raster) <- "ctry_id"

SECTOR_INDEX <- which(HELPS::SECTOR_ALL == "MAIZ_R")
smw <- HELPS::SECTOR_SMW[[SECTOR_INDEX]]
smw.df <- smw %>% as.data.frame(xy = T) %>% setNames(c("x", "y", "smw"))

pwc.hothaps.ann %>% rename(PWC = value) %>% head
country_raster %>% as.data.frame(xy = T) -> country_raster.df
reg_WB_raster %>% as.data.frame(xy = T) -> reg_WB_raster.df

agg.df <- pwc.hothaps.ann %>% rename(PWC = value)

identical(agg.df$x, reg_WB_raster.df$x, country_raster.df$x, smw.df$x)
identical(agg.df$y, reg_WB_raster.df$y, country_raster.df$y, smw.df$y)

agg.df$smw <- smw.df$smw
agg.df$ctry_id <- country_raster.df$ctry_id
agg.df$glu_id <- reg_WB_raster.df$glu_id

agg.df %>%
  group_by(ctry_id) %>%
  summarise(PWC = weighted.mean(PWC, smw, na.rm = T)) ->
  ctry_pwc

agg.df %>%
  group_by(glu_id) %>%
  summarise(PWC = weighted.mean(PWC, smw, na.rm = T)) ->
  glu_pwc

## ----Step3.1, warning=FALSE, fig.width=10, fig.height=6-----------------------
GCAM_country %>% left_join(ctry_pwc, by = "ctry_id") %>%
  ggplot() +
  geom_sf(aes(fill = 100 * (1-PWC))) +
  scale_fill_gradient(low = "white", high = "red",
                      na.value = "grey85")+
  labs(x = "longitude", y = "latitude", fill = "PWC \nloss(%)",
       title = "Country-level heat-induced PWC loss for labor in rain-fed maize sector: 2024") +
  theme_bw()

## ----Step3.2, warning=FALSE, fig.width=10, fig.height=6-----------------------
reg_WB %>% left_join(glu_pwc, by = "glu_id") %>%
  ggplot() +
  geom_sf(aes(fill = 100 * (1-PWC))) +
  scale_fill_gradient(low = "white", high = "red",
                      na.value = "grey85")+
  labs(x = "longitude", y = "latitude", fill = "PWC \nloss(%)",
       title = "Water basin-level heat-induced PWC loss for labor in rain-fed maize sector: 2024") +
  theme_bw()

