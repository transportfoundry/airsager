#' Disaggregate from districts to zones.  Creates separate tables
#' for each subscriber class and time of day.
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
#' @param centroids SpatialPointsDataFrame of model centroids.  Can
#'    include the external stations (use the model node layer).
#'    Must also contain the following fields:
#'    \describe{
#'    \item{ZONEID}{Unique identifier for each row/zone}
#'    \item{EXTSTATION}{Externals stations marked with a 1}
#'    \item{SE}{"Stuff" in the zone to use for weighted disaggregation. Can be
#'    any combination of residential and/or employment info.
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
#' @param tod_equiv Named list specifying the TOD used by the AirSage data.
#'    For example:
#'    \itemize{
#'      \item EA = "H00:H06"
#'      \item AM = "H06:H09"
#'      \item MD = "H09:H15"
#'      \item PM = "H15:H18"
#'      \item EV = "H18:H24"
#'    }
#' @param ee_filter Dataframe with a `from` and `to` column. Removes flows from
#'    included district pairs. Used to remove EE flows that do not enter the
#'    model region.
#' @return A tbl_df() object the contains the trips from origin zones to
#'    destination zones.  The functions also writes out tables to your working
#'    directory.

as_disagg <- function(asTable, centroids, districts, tod_equiv, ee_filter = NA){

  # Remove adjacent external zone trips
  asTable <- remove_adjacent_trips(asTable, centroids, districts, ee_filter)

  # Create an equivalency layer
  equivLyr <- make_equivLyr(centroids, districts)

  # Calculate zonal percentages of districts based on the SE field
  equivLyr <- calc_perc(equivLyr)

  # Disaggregate into zones
  expTbl <- explode(asTable$corrected_flows, equivLyr)

  # Clean table and write out mini CSVs
  expTbl <- format(expTbl, tod_equiv)

  return(expTbl)
}



#' Create Equivalency Layer
#'
#' @inheritParams as_disagg
#' @return A SpatialPointsDataFrame of TAZ centroids with the District
#'    ID appended.

make_equivLyr <- function(centroids, districts) {

  # Overlay the two layers
  temp <- sp::over(centroids, districts)

  # Create output equivalency table
  centroids$DISTRICTID <- temp$DISTRICTID

  return(centroids)
}


#' Calculate the percent each centroid makes up of the district
#'
#' @param equivLyr Equivalency Layer created by make_equivLyr()
#'
#' @importFrom magrittr "%>%"

calc_perc <- function(equivLyr){

  tbl <- equivLyr@data

  tbl <- tbl %>%
    dplyr::mutate(
      EXTSTATION = ifelse(is.na(EXTSTATION), 0, EXTSTATION),
      SE = ifelse(is.na(SE), 0, SE),
      VOLUME = ifelse(is.na(VOLUME), 0, VOLUME)
    ) %>%
    dplyr::group_by(DISTRICTID) %>%
    dplyr::mutate(
      PERCENT = SE / sum(SE),
      PERCENT = ifelse(is.na(PERCENT), VOLUME / sum(VOLUME), PERCENT)
    ) %>%
    dplyr::ungroup()

  equivLyr@data$PERCENT <- tbl$PERCENT

  return(equivLyr)
}

#' Explodes the district-to-district table into a zone-to-zone table
#'
#' @inheritParams as_disagg
#' @param equivLyr Equivalency layer updated by calc_perc()
#' @importFrom magrittr "%>%"
#' @return expTbl Exploded, zonal table

explode <- function(asTable, equivLyr){

  # Simplify the equivLyr into just the fields needed
  # (No longer a spatial layer)
  equivTbl <- equivLyr@data %>%
    dplyr::select(DISTRICTID, ZONEID, PERCENT)

  # Create the exploded table by joining the equivTbl twice.
  # First join based on Origin; second based on Destination
  expTbl <- asTable %>%
    dplyr::left_join(equivTbl, by = stats::setNames("DISTRICTID", "Origin_Zone")) %>%
    dplyr::rename(OrigCentroid = ZONEID, OrigPct = PERCENT) %>%
    dplyr::left_join(equivTbl, by = stats::setNames(
      "DISTRICTID", "Destination_Zone")
      ) %>%
    dplyr::rename(DestCentroid = ZONEID, DestPct = PERCENT) %>%
    dplyr::mutate(
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
      "During disaggregation, trips went from ", before, " to ",
      after, ". A difference of ", diff, " or ", pctdiff, "%."
      )
    )

  return(expTbl)
}


#' Formats columns and values in the exploded table.
#'
#' @inheritParams as_disagg
#' @param expTbl Exploded, zonal table returned by explode
#' @importFrom magrittr "%>%"
#' @return Returns the final table.

format <- function(expTbl, tod_equiv){

  # Convert the tod_equiv to a data frame
  tod_equiv <- as.data.frame(tod_equiv) %>%
    gather(key = period, value = as_period)

  # Format table
  expTbl <- expTbl %>%
    dplyr::select(FROM = OrigCentroid, TO = DestCentroid,
                 RESIDENT = Subscriber_Class,
                 TOD = Time_of_Day, Purpose, FinalTrips) %>%
    dplyr::filter(!is.na(FROM), !is.na(TO)) %>%
    dplyr::left_join(tod_equiv, by = stats::setNames("as_period", "TOD")) %>%
    dplyr::mutate(TOD = period) %>%
    dplyr::select(-period) %>%
    tidyr::unite(PURPTOD, Purpose, TOD, sep = "_") %>%
    tidyr::spread(PURPTOD, FinalTrips)

  expTbl[is.na(expTbl)] <- 0

  return(expTbl)
  
}



