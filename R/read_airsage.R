#' Read an AirSage trip matrix
#'
#' @param file Path to an AirSage CSV file
#'
#' @return A tibble with AirSage values coded in and tidy formats
#'
#' @importFrom readr read_csv col_integer col_date col_double
#' 
#' @export
#'
read_airsage <- function(file){

  # read the file
  readr::read_csv(
    file,

    col_types = list(
      Origin_Zone = readr::col_integer(),
      Destination_Zone = readr::col_integer(),
      Start_Date = readr::col_date(format = "%Y%m%d"),
      End_Date = readr::col_date(format = "%Y%m%d"),
      Count = readr::col_double()
    )
  )

}
