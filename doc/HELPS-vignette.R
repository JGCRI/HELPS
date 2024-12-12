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

## ----Step0, warning=FALSE, eval = FALSE---------------------------------------
# get_example_data()

## ----Step1.1, warning=FALSE---------------------------------------------------
wbgt.sun.day <- HeatStress(TempRes = "day", SECTOR = "MAIZ_I", HS = WBGT_sun, YEAR_INPUT = 2024,
                    "../HELPS_Example_Data/hurs_example_day.nc",
                    "../HELPS_Example_Data/tas_example_day.nc",
                    "../HELPS_Example_Data/ps_example_day.nc")
wbgt.sun.day

## ----Step1.2, warning=FALSE---------------------------------------------------
pwc.hothaps.day <- PWC(WBGT = wbgt.sun.day,  LHR = LHR_Hothaps, workload = "high")
pwc.hothaps.day

## ----Step1.3, warning=FALSE---------------------------------------------------
wbgt.sun.mon <- DAY2MON(input_rack = wbgt.sun.day)
rm(wbgt.sun.day)
pwc.hothaps.mon <- DAY2MON(input_rack = pwc.hothaps.day)
rm(pwc.hothaps.day)
wbgt.sun.mon
pwc.hothaps.mon


## ----Step1.4, warning=FALSE---------------------------------------------------
wbgt.sun.ann <- MON2ANN(input_rack = wbgt.sun.mon, SECTOR = "MAIZ_I")
pwc.hothaps.ann <- MON2ANN(input_rack = pwc.hothaps.mon, SECTOR = "MAIZ_I")

summary(wbgt.sun.ann)
summary(pwc.hothaps.ann)

## ----Step2.1, warning=FALSE---------------------------------------------------
esi.mon <- HeatStress(TempRes = "month", SECTOR = "MAIZ_I", HS = WBGT_ESI, YEAR_INPUT = 2024,
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
pwc.hothaps.ann2 <- MON2ANN(input_rack = pwc.mon.hothaps, SECTOR = "MAIZ_I")
summary(pwc.hothaps.ann2)

