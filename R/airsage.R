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
  expTbl <- explode(asTable, equivLyr)

  # Clean table and write out mini CSVs
  expTbl <- format(expTbl, tod_equiv)

  return(expTbl)
}

#' Remove adjacent external zone trips.  External trips to adjacent external
#' zones should not be loaded onto the region's model network.  Similarly,
#' external trips made within the same external zone are also removed.
#'
#' @inheritParams as_disagg
#' @importFrom magrittr "%>%"

remove_adjacent_trips <- function(asTable, centroids, districts, ee_filter) {

  # Overlay the two layers to determine which districts are external
  # This can miss an external AS district if there are no modeled external
  # stations in it.
  temp <- sp::over(districts, centroids)
  districts$EXTSTATION <- temp$EXTSTATION
  districts$EXTSTATION[is.na(districts$EXTSTATION)] <- 0
  extTbl <- districts@data %>%
    dplyr::select(DISTRICTID, EXTSTATION)

  # Modify asTable to mark which districts are external stations
  asTable <- asTable %>%
    dplyr::left_join(extTbl, by = c("Origin_Zone" = "DISTRICTID")) %>%
    dplyr::rename(EXTORIGIN = EXTSTATION) %>%
    dplyr::left_join(extTbl, by = c("Destination_Zone" = "DISTRICTID")) %>%
    dplyr::rename(EXTDESTINATION = EXTSTATION)

  # Remove intrazonal trips from external stations
  remIntTrips <- asTable %>%
    dplyr::filter(EXTORIGIN == 1, Origin_Zone == Destination_Zone) %>%
    dplyr::summarise(total = sum(Count)) %>%
    as.numeric()
  print(paste0(
    "Removing ", remIntTrips, " trips that are intrazonal for external zones"
  ))
  asTable <- asTable %>%
    dplyr::mutate(
      Count = ifelse(EXTORIGIN == 1 & Origin_Zone == Destination_Zone,
                     0, Count)
    )

  # Determine neighbors
  ext_dists <- districts[districts$EXTSTATION == 1, ]
  adj <- spdep::poly2nb(ext_dists)
  temp <- spdep::nb2mat(adj, style = "B")
  rownames(temp) <- ext_dists$DISTRICTID
  colnames(temp) <- ext_dists$DISTRICTID

  adjTbl <- temp %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(FROM = rownames(temp)) %>%
    tidyr::gather(key = TO, value = ADJ, -FROM) %>%
    dplyr::mutate(FROM = as.numeric(FROM), TO = as.numeric(TO))

  # Modify asTable to remove adjacent external trips
  asTable <- asTable %>%
    dplyr::left_join(adjTbl, by = stats::setNames(
      c("FROM", "TO"),
      c("Origin_Zone", "Destination_Zone")
    )) %>%
    dplyr::mutate(ADJ = ifelse(is.na(ADJ), 0, ADJ))

  remAdjTrips <- sum(asTable$Count[asTable$ADJ == 1])
  print(paste0(
    "Removing ", remAdjTrips, " that are from external stations to ",
    "adjacent external stations"
  ))
  asTable <- asTable %>%
    dplyr::mutate(
      Count = ifelse(ADJ == 1, 0, Count)
    )

  # If it exists, use the ee_filter to remove additional flows as specified
  if (is.data.frame(ee_filter)){
    # standardize the column names
    names <- colnames(ee_filter)
    names[1] <- "from"
    names[2] <- "to"
    colnames(ee_filter) <- names

    # Create a percent column if one isn't present
    if (is.null(ee_filter$percent)){
      ee_filter$percent <- 0
    }

    # make sure each row in the ee_filter is unique
    ee_filter <- ee_filter %>%
      group_by(from, to) %>%
      summarize(percent = mean(percent)) %>%
      ungroup()

    # join ee_filter to asTable
    asTable <- asTable %>%
      dplyr::left_join(ee_filter, by = stats::setNames(
        c("from", "to"),
        c("Origin_Zone", "Destination_Zone")
      ))

    # print out how many trips will be removed by the ee_filter
    rem_ee_filter <- asTable %>%
      dplyr::filter(!is.na(percent)) %>%
      dplyr::summarize(Count = sum(Count)) %>%
      .$Count
    print(paste0(
      "Removing ", rem_ee_filter, " additional trips according to ",
      "the ee_filter"
    ))

    # remove trips
    asTable <- asTable %>%
      mutate(Count = ifelse(!is.na(percent), Count * percent, Count)) %>%
      select(-percent)
  }

  return(asTable)
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

  # Format table
  expTbl <- expTbl %>%
    dplyr::select(FROM = OrigCentroid, TO = DestCentroid,
                 RESIDENT = Subscriber_Class,
                 TOD = Time_of_Day, Purpose, FinalTrips) %>%
    dplyr::filter(!is.na(FROM), !is.na(TO)) %>%
    dplyr::mutate(
      TOD = ifelse(TOD == tod_equiv$EA, "EA", TOD),
      TOD = ifelse(TOD == tod_equiv$AM, "AM", TOD),
      TOD = ifelse(TOD == tod_equiv$MD, "MD", TOD),
      TOD = ifelse(TOD == tod_equiv$PM, "PM", TOD),
      TOD = ifelse(TOD == tod_equiv$EV, "EV", TOD),
      TOD = ifelse(TOD == "H00:H24", "Daily", TOD)
    ) %>%
    tidyr::unite(PURPTOD, Purpose, TOD, sep = "_") %>%
    tidyr::spread(PURPTOD, FinalTrips)

  expTbl[is.na(expTbl)] <- 0

  return(expTbl)
}

