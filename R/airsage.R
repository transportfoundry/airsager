#' Create Equivalency Table
#'
#' @param modelZones Path to a shapefile of model zones.  Must
#'    include the external stations, too.  Use the node layer.
#'    Must also contain the following fields:
#'    \describe{
#'    \item{zID}{Unique identifier for each row}
#'    \item{Ext}{Externals stations marked with a 1}
#'    \item{SE}{"Stuff" in the zone to use for disaggregation. Can be any
#'    combination of residential and/or employment info.
#'    For example:
#'    \itemize{
#'    \item SE = Households + Employment
#'    \item SE = Number of Parcels
#'    \item etc.
#'    }}
#'    \item {Volume}{For external stations, this column will contain a measure
#'    of the volume/count at that station.}
#'    }
#' @param tazID TAZ ID Number
#' @param asDistricts Path to a shapefile of AirSage districts.
#'    Must be a polygon layer.
#' @param distID The district ID that will be tagged to the zones
#' @return A data frame with a row for each model zone specifying
#'    it's AirSage district

make_equivTbl <- function(modelZones, tazID, asDistricts, distID) {

  # Read in the shapefiles
  layer <- tools::file_path_sans_ext(basename(asDistricts))
  dsn <- dirname(asDistricts)
  dist <- readOGR(dsn = dsn, layer = layer, verbose = FALSE)
  layer <- tools::file_path_sans_ext(basename(modelZones))
  dsn <- dirname(modelZones)
  taz <- readOGR(dsn = dsn, layer = layer, verbose = FALSE)

  # Set identical projection info
  proj4string(dist) <- proj4string(taz)

  # Overlay the two layers
  temp <- over(taz, dist)

  # Create output equivalency table
  result <- data.frame(taz@data[tazID], temp[distID])
}

#' Disaggregate from districts to zones.  Creates separate tables
#' for each subscriber class, purpose, and time of day.
#'
#' @param asTable Table of AirSage data as provided in their
#'    format.  Should have 9 fields:
#'    \itemize{
#'    \item Origin_Zone
#'    \item Destination_Zone
#'    \item Start_Date
#'    \item End_Date
#'    \item Aggregation
#'    \item Subscriber_Class
#'    \item Purpose
#'    \item Time_of_Day
#'    \item Count
#'    }
#' @param equivTbl Equivalency table created by make_equivTbl()

as_disagg <- function(asTable, equivTbl){

}






