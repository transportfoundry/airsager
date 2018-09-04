#' Split external zone trips between stations
#' 
#' The AirSage zones will typically have several external stations within
#' each zone. This function distributes these trips between the stations
#' according to the AWDT observed at each station. It does not rebalance
#' the flows to match the AWDT.
#' 
#' @param asTable An AirSage table from which adjacent zone external trips
#'   have already been removed.
#' @param ext_stations A data_frame representing external stations with three
#'   columns: 
#'     - `n` The centroid ID in the highway network
#'     - `zone` The AirSage zone the node is in
#'     - `awdt` The traffic count at the external stations
#' 
#' @importFrom dplyr group_by mutate left_join rename
#' @export
#' 
split_external_stations <- function(asTable, ext_stations){
  
  asTable %>% ungroup() %>%
    dplyr::group_by(
      Origin_Zone, Destination_Zone, Subscriber_Class, Purpose, Time_of_Day
    ) %>%
    
    # Distribute trips between stations on the origin side
    # Join the stations data frame to the flows data. The count at 
    # the external station needs to be divided in half because the other
    # piece of its AWDT will be distributed from the other stations.
    dplyr::left_join(
      ext_stations %>% transmute(o_n = n, o_z = zone, o_awdt = awdt / 2), 
      by = c("Origin_Zone" = "o_z")) %>%
    dplyr::mutate(
      Count = ifelse(as.logical(EXTORIGIN), Count * o_awdt / sum(o_awdt), Count)
    ) %>%
    
    # Distribute trips between stations on the destination side
    # Join the stations data frame to the flows data, accounting for the other
    # half of the traffic counts. Add origin node to the grouping categories
    # and redistribute.
    dplyr::left_join(
      ext_stations %>% transmute(d_n = n, d_z = zone, d_awdt = awdt / 2),  
      by = c("Destination_Zone" = "d_z")) %>%
    dplyr::group_by(Origin_Zone, Destination_Zone, Subscriber_Class, 
             Purpose, Time_of_Day, o_n) %>%
    dplyr::mutate(
      Count = ifelse(as.logical(EXTDESTINATION), 
                     Count * d_awdt / sum(d_awdt), Count)
    ) %>%
    
    dplyr::rename(Origin_Station = o_n, Destination_Station = d_n)

}