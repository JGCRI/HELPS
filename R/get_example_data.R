#' get_example_data
#'
#' Function to download the test data files from zenodo that are needed to run HELPS.
#'
#' @param write_dir Default = getwd()
#' @param dir_name = "HELPS_Example_Data". Name of directory to install zip file to.
#' @param data_link Default =
#' c(""https://zenodo.org/records/14270969/files/daily_inputs_example.zip?download=1",
#' "https://zenodo.org/records/14270969/files/monthly_inputs_example.zip?download=1")
#' @keywords test
#' @return path with example input data
#' @importFrom rlang :=
#' @importFrom dplyr %>%
#' @export
#' @examples
#' \dontrun{
#' library(HELPS)
#' HELPS::get_example_data()
#' }

get_example_data <- function(write_dir = getwd(),
                             dir_name = "HELPS_Example_Data",
                             data_links = c("https://zenodo.org/records/14270969/files/monthly_inputs_example.zip?download=1",
                                            "https://zenodo.org/records/14270969/files/daily_inputs_example.zip?download=1")) {

  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting get_example_data")

  # Set timeout globally once
  options(timeout = max(300, getOption("timeout")))

  # Create the directory for example data
  target_dir <- paste0(write_dir, "/", dir_name)

  if (!dir.exists(paths = target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  for (i in seq_along(data_links)) {
    # Generate unique names for downloaded zip files
    zip_name <- paste0(target_dir, "/example_data_", i, ".zip")

    # Download the zip file
    rlang::inform(paste0("Starting download of file ", i, "..."))

    utils::download.file(url = data_links[i],
                         destfile = zip_name,
                         mode = "wb")

    rlang::inform(paste0("Download of file ", i, " complete."))

    # Unzip the file
    rlang::inform(paste0("Starting unzip of file ", i, "..."))

    utils::unzip(zip_name, exdir = target_dir)

    # Remove the zip file after extraction
    unlink(zip_name)

    rlang::inform(paste0("Unzip of file ", i, " complete."))
  }

  rlang::inform(paste0("The example data was installed to: ", target_dir,
                       ". You can use input files in \"", target_dir, "\" to run the example scripts from the package vignette."))

  #.........................
  # Close Out
  #.........................

  rlang::inform("get_example_data completed.")

  return(target_dir)
}
