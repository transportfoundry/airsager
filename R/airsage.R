#' Disaggregate from districts to zones.  Creates separate tables
#' for each subscriber class, purpose, and time of day.
#'
#' @export
#' @param asTable Table of AirSage data as provided in their standard
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
#' @param centroids SpatialPointsDataFrame of model centroids.  Must
#'    include the external stations (use the model node layer).
#'    Must also contain the following fields:
#'    \describe{
#'    \item{ZONEID}{Unique identifier for each row/zone}
#'    \item{EXTSTATION}{Externals stations marked with a 1}
#'    \item{SE}{"Stuff" in the zone to use for disaggregation. Can be any
#'    combination of residential and/or employment info.
#'    For example:
#'    \itemize{
#'      \item SE = Households + Employment
#'      \item SE = Number of Parcels
#'      \item etc.
#'    }}
#'    \item{VOLUME}{For external stations, this column will contain a measure
#'    of the volume/count at that station.}
#'    }
#' @param districts SpatialPointsDataFrame of AirSage districts. Must be a polygon
#'    layer.  Must contain the following field:
#'    \describe{
#'      \item{DISTRICTID}{Unique identifier for each row/district}
#'    }
#' @return A tbl_df() object the contains the trips from origin zones to
#'    destination zones.

as_disagg <- function(asTable, centroids, districts){

  # Plot the centroids and districts for the user to see
  proj4string(districts) <- proj4string(centroids)
  plot(districts)
  points(centroids)

  # Create an equivalency layer
  equivLyr <- make_equivLyr(centroids, districts)

  # Calculate zonal percentages of districts based on the SE field
  equivLyr <- calc_perc(equivLyr)

  # Disaggregate into zones
  expTbl <- explode(asTable, equivLyr)

  return(expTbl)
}


#' Create Equivalency Layer
#'
#' @inheritParams as_disagg
#' @return A SpatialPointsDataFrame of TAZ centroids with the District
#'    ID appended.

make_equivLyr <- function(centroids, districts) {

  # Overlay the two layers
  temp <- over(centroids, districts)

  # Create output equivalency table
  centroids@data$DISTRICTID <- temp$DISTRICTID

  return(centroids)
}


#' Calculate the percent each centroid makes up of the district
#'
#' @param equivLyr Equivalency Layer created by make_equivLyr()

calc_perc <- function(equivLyr){

  tbl <- equivLyr@data

  tbl <- tbl %>%
    mutate(
      EXTSTATION = ifelse(is.na(EXTSTATION), 0, EXTSTATION),
      SE = ifelse(is.na(SE), 0, SE),
      VOLUME = ifelse(is.na(VOLUME), 0, VOLUME)
    ) %>%
    group_by(DISTRICTID) %>%
    mutate(
      PERCENT = SE / sum(SE),
      PERCENT = ifelse(is.na(PERCENT), VOLUME / sum(VOLUME), PERCENT)
    ) %>%
    ungroup()

  equivLyr@data$PERCENT <- tbl$PERCENT

  return(equivLyr)
}

#' Explodes the district-to-district table into a zone-to-zone table
#'
#' @inheritParams as_disagg
#' @param equivLyr Equivalency layer updated by calc_perc()

explode <- function(asTable, equivLyr){

  # Simplify the equivLyr into just the fields needed
  # (No longer a spatial layer)
  equivTbl <- equivLyr@data %>%
    select(DISTRICTID, ZONEID, PERCENT)

  # Create the exploded table by joining the equivTbl twice.
  # First join based on Origin; second based on Destination
  expTbl <- asTable %>%
    left_join(equivTbl, by = c("Origin_Zone" = "DISTRICTID")) %>%
    rename(OrigCentroid = ZONEID, OrigPct = PERCENT) %>%

    left_join(equivTbl, by = c("Destination_Zone" = "DISTRICTID")) %>%
    rename(DestCentroid = ZONEID, DestPct = PERCENT) %>%
    mutate(
      FinalPct = OrigPct * DestPct,
      FinalTrips = Count * FinalPct
    )

  # Report trip conservation
  before <- sum(asTable$Count, na.rm = TRUE)
  after <- sum(expTbl$FinalTrips, na.rm = TRUE)
  diff <- round(after - before, 2)
  pctdiff <- round(diff / before * 100, 2)
  print(
    paste0(
      "Trips went from ", before, " to ", after, ". A difference of ", diff,
      " or ", pctdiff, "%."
      )
    )

  return(expTbl)
}





