# download the monthly input for tests
write_dir = getwd()
dir_name = "HELPS_Test_Data"
target_dir <- paste0(write_dir, "/", dir_name)
zip_name <- paste0(target_dir, "/test_data.zip"); zip_name

# Download the zip file
utils::download.file(url = "https://zenodo.org/records/14270969/files/monthly_inputs_example.zip?download=1",
                     destfile = zip_name, mode = "wb")
utils::unzip(zip_name, exdir = target_dir)
# Remove the zip file after extraction
unlink(zip_name)

# ------------------------------------
# Testing Outputs from Major Functions
# ------------------------------------

testthat::test_that("key function tests", {
  esi.mon <- HeatStress(TempRes = "month", SECTOR = "MAIZ_R", HS = WBGT_ESI, YEAR_INPUT = 2024,
                        paste0(target_dir,"/hurs_example_month.nc"),
                        paste0(target_dir,"/tas_example_month.nc"),
                        paste0(target_dir,"/rsds_example_month.nc"))
  testthat::expect_snapshot(esi.mon)

  pwc.mon.foster <- PWC(WBGT = esi.mon,  LHR = LHR_Foster, workload = "high")
  testthat::expect_snapshot(pwc.mon.foster)

  pwc.foster.ann <- MON2ANN(input_rack = pwc.mon.foster, SECTOR = "MAIZ_R")
  testthat::expect_snapshot(pwc.foster.ann)

  ctry_pwc <- G2R(grid_annual_value = pwc.foster.ann, SECTOR = "MAIZ_R", rast_boundary = country_raster)
  testthat::expect_snapshot(ctry_pwc)
})

# testthat::test_that("monthly LHR_Foster test", {
#   esi.mon <- HeatStress(TempRes = "month", SECTOR = "MAIZ_R", HS = WBGT_ESI, YEAR_INPUT = 2024,
#                         paste0(target_dir,"/hurs_example_month.nc"),
#                         paste0(target_dir,"/tas_example_month.nc"),
#                         paste0(target_dir,"/rsds_example_month.nc"))
#   pwc.mon.foster <- PWC(WBGT = esi.mon,  LHR = LHR_Foster, workload = "high")
#
#   testthat::expect_snapshot(pwc.mon.foster)
# })
#
# testthat::test_that("month to anual test", {
#   pwc.foster.ann <- MON2ANN(input_rack = pwc.mon.foster, SECTOR = "MAIZ_R")
#   testthat::expect_snapshot(pwc.foster.ann)
# })
#
# testthat::test_that("grid to region test", {
#   ctry_pwc <- G2R(grid_annual_value = pwc.foster.ann, SECTOR = "MAIZ_R", rast_boundary = country_raster)
#   testthat::expect_snapshot(ctry_pwc)
# })
#
#
#
