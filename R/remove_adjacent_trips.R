#' Remove adjacent external zone trips  
#' 
#' External trips to adjacent external zones should not be loaded onto the
#' region's model network.  Similarly, external trips made within the same
#' external zone are also removed.
#'
#' @return A `list` with four elements:
#'   - `corrected_flows` A `data_frame` of the same structure as `asTable`, but
#'   with adjacent and intra-district AirSage flows removed.
#'   - `original_external_trips` The original number of external zone trips.
#'   - `intrazonal_external_trips` The number of removed intrazonal trips in
#'   external districts.
#'   - `adjacent_external_trips` The number of adjacent-zone flows removed
#'   from the source data.
#'   
#' @details If the `centroids` layer is not provided, then there must be a
#'   field in `districts$EXTSTATION` identifying external station zones with
#'   a value of 1 for external zones and 0 for internal zones.
#'
#' @inheritParams as_disagg
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 
remove_adjacent_trips <- function(asTable, centroids = NULL, districts, 
                                  ee_filter = NULL) {
  
  # If the centroids layer is given, use that to determine which zones are
  # external. Otherwise, expect that the externals field will be supplied on the
  # districts shape object.
  if (is.null(centroids)){
    if (is.null(districts$EXTSTATION)) 
      stop("Districts layer must contain EXTSTATION")
    if (is.null(districts$DISTRICTID)) 
      stop("Districts layer must contain DISTRICTID")
  } else {
    # Overlay the two layers to determine which districts are external
    # This can miss an external AS district if there are no modeled external
    # stations in it.
    temp <- sp::over(districts, centroids)
    districts$EXTSTATION <- temp$EXTSTATION
    districts$EXTSTATION[is.na(districts$EXTSTATION)] <- 0
  }
  
  extTbl <- districts@data %>%
    dplyr::select(DISTRICTID, EXTSTATION)
  
  # continue only if external districts were found
  if (sum(extTbl$EXTSTATION) > 0){
    
    
    # Modify asTable to mark which districts are external stations
    asTable <- asTable %>%
      dplyr::left_join(extTbl, by = c("Origin_Zone" = "DISTRICTID")) %>%
      dplyr::rename(EXTORIGIN = EXTSTATION) %>%
      dplyr::left_join(extTbl, by = c("Destination_Zone" = "DISTRICTID")) %>%
      dplyr::rename(EXTDESTINATION = EXTSTATION)
    
    # count total external flows in AirSage 
    total_trips <- asTable %>%
      dplyr::filter(EXTORIGIN == 1 | EXTDESTINATION == 1) %>%
      dplyr::pull(Count) %>%
      sum()
    message("There are ", prettyNum(total_trips, big.mark = ","), 
            " total external district trips.")
    
    
    # Remove intrazonal external zone trips
    remIntTrips <- asTable %>%
      dplyr::filter(EXTORIGIN == 1 & Origin_Zone == Destination_Zone) %>%
      dplyr::pull(Count) %>% sum() 
    
    message("Removing ", prettyNum(remIntTrips, big.mark = ","),
            " trips that are intrazonal for external zones")
    
    asTable <- asTable %>%
      dplyr::mutate(
        Count = ifelse(
          EXTORIGIN == 1 & Origin_Zone == Destination_Zone, 
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
    message("Removing ", prettyNum(remAdjTrips, big.mark = ","), 
            " trips that are from external stations to adjacent external stations")
    asTable <- asTable %>%
      dplyr::mutate( Count = ifelse(ADJ == 1, 0, Count) ) 
    
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
      message("Removing ", prettyNum(rem_ee_filter, big.mark = ","), 
              " additional trips according to the ee_filter")
      
      # remove trips
      asTable <- asTable %>%
        mutate(Count = ifelse(!is.na(percent), Count * percent, Count)) %>%
        select(-percent)
      
    }
  }
  
  total_remaining_trips <- asTable %>% 
    dplyr::filter(EXTORIGIN == 1 | EXTDESTINATION == 1) %>%
    dplyr::pull(Count) %>% sum()
  message("There remain ", prettyNum(total_remaining_trips, big.mark = ","), 
          " external trips.")
  
  list(
    corrected_flows = asTable, 
    original_external_trips = total_trips,
    intrazonal_external_trips = remIntTrips,
    adjacent_external_trips = remAdjTrips
  )
  
}
