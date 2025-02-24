#' find_transit_candidates
#' 
#' @import tidygraph
#' 
#' @noRd
find_transit_candidates = function(transit_grid, freq_bus) {
  
  if (max(transit_grid$frequency, na.rm = TRUE) == 0){
    
    transit_candidates = transit_grid |> 
      mutate(transit = 0,
             transit_candidate = 0)
    
  } else {
    
    transit_candidates = transit_grid |> 
      mutate(transit = case_when(
        frequency <= freq_bus[1] ~ 1,
        frequency > freq_bus[1] & frequency <= freq_bus[2] ~ 2,
        frequency > freq_bus[2] & frequency <= freq_bus[3] ~ 3,
        frequency > freq_bus[3] ~ 4
      )) |> 
      mutate(transit_candidate = ifelse(transit %in% c(3,4), 1, 0))
  }
  
  return(transit_candidates)
  
}