#' export_analysis
#' 
#' @import tidygraph
#' @import openxlsx
#' @import dplyr
#' 
#' @noRd
export_analysis = function(grid_all, grid_selection, CITY_input, GEOJSON, GEOJSON_input, analysis,
                           cellsize_input, square_input, use_h3, h3_res, build_osm, degree_min, betweeness_range,
                           closeness_range, population_min, entropy_min, freq_bus) {
  
  if (analysis == TRUE){
    
    print("Exporting analysis...")
    
    analysis_row = data.frame(timestamp = Sys.time()) |> 
      mutate(CITY = ifelse(GEOJSON == FALSE, CITY_input, NA),
             GEOJSON_name = ifelse(GEOJSON == TRUE, GEOJSON_input, NA),
             square = ifelse(use_h3 == FALSE, as.character(square_input), "hex h3"),
             h3_res = ifelse(use_h3 == TRUE, h3_res, NA),
             cellsize_a = ifelse(use_h3 == FALSE, cellsize_input[1], NA),
             cellsize_b = ifelse(use_h3 == FALSE, cellsize_input[2], NA),
             build_osm = build_osm,
             degree_min = ifelse(methods(degree_min)[1] == "mean.Date", "mean", "median"),
             betweeness_range = betweeness_range,
             closeness_range = closeness_range,
             population_min = ifelse(methods(population_min)[1] == "mean.Date", "mean", "median"),
             entropy_min = entropy_min,
             freq_bus_1 = freq_bus[1],
             freq_bus_2 = freq_bus[2],
             freq_bus_3 = freq_bus[3]) |>
      mutate(n_candidates = nrow(grid_all),
             n_degree = sum(grid_all$degree_candidate, na.rm = TRUE),
             n_betweenness = sum(grid_all$betweenness_candidate, na.rm = TRUE),
             n_closeness = sum(grid_all$closeness_candidate, na.rm = TRUE),
             n_population = sum(grid_all$population_candidate, na.rm = TRUE),
             n_entropy = sum(grid_all$entropy_candidate, na.rm = TRUE),
             n_transit = sum(grid_all$transit_candidate, na.rm = TRUE),
             n_transit_3 = sum(grid_all$transit == 3, na.rm = TRUE),
             n_transit_4 = sum(grid_all$transit == 4, na.rm = TRUE),
             n_selected = nrow(grid_selection),
             n_complex = sum(grid_selection$complexity == "complex"),
             n_very_complex = sum(grid_selection$complexity == "very complex")) |> 
      mutate(mean_degree = mean(grid_all$degree, na.rm = TRUE),
             sd_degree = sd(grid_all$degree, na.rm = TRUE),
             min_degree = min(grid_all$degree, na.rm = TRUE),
             q1_degree = quantile(grid_all$degree, 0.25, na.rm = TRUE),
             median_degree = median(grid_all$degree, na.rm = TRUE),
             q3_degree = quantile(grid_all$degree, 0.75, na.rm = TRUE),
             max_degree = max(grid_all$degree, na.rm = TRUE)) |> 
      mutate(mean_betweenness = mean(grid_all$betweenness, na.rm = TRUE),
             sd_betweenness = sd(grid_all$betweenness, na.rm = TRUE),
             min_betweenness = min(grid_all$betweenness, na.rm = TRUE),
             q1_betweenness = quantile(grid_all$betweenness, 0.25, na.rm = TRUE),
             median_betweenness = median(grid_all$betweenness, na.rm = TRUE),
             q3_betweenness = quantile(grid_all$betweenness, 0.75, na.rm = TRUE),
             max_betweenness = max(grid_all$betweenness, na.rm = TRUE)) |>
      mutate(mean_closeness = mean(grid_all$closeness, na.rm = TRUE),
             sd_closeness = sd(grid_all$closeness, na.rm = TRUE),
             min_closeness = min(grid_all$closeness, na.rm = TRUE),
             q1_closeness = quantile(grid_all$closeness, 0.25, na.rm = TRUE),
             median_closeness = median(grid_all$closeness, na.rm = TRUE),
             q3_closeness = quantile(grid_all$closeness, 0.75, na.rm = TRUE),
             max_closeness = max(grid_all$closeness, na.rm = TRUE)) |>
      mutate(mean_population = mean(grid_all$population, na.rm = TRUE),
             sd_population = sd(grid_all$population, na.rm = TRUE),
             min_population = min(grid_all$population, na.rm = TRUE),
             q1_population = quantile(grid_all$population, 0.25, na.rm = TRUE),
             median_population = median(grid_all$population, na.rm = TRUE),
             q3_population = quantile(grid_all$population, 0.75, na.rm = TRUE),
             max_population = max(grid_all$population, na.rm = TRUE)) |>
      mutate(mean_entropy = mean(grid_all$entropy, na.rm = TRUE),
             sd_entropy = sd(grid_all$entropy, na.rm = TRUE),
             min_entropy = min(grid_all$entropy, na.rm = TRUE),
             q1_entropy = quantile(grid_all$entropy, 0.25, na.rm = TRUE),
             median_entropy = median(grid_all$entropy, na.rm = TRUE),
             q3_entropy = quantile(grid_all$entropy, 0.75, na.rm = TRUE),
             max_entropy = max(grid_all$entropy, na.rm = TRUE)) |>
      mutate(mean_frequency = mean(grid_all$frequency, na.rm = TRUE),
             sd_frequency = sd(grid_all$frequency, na.rm = TRUE),
             min_frequency = min(grid_all$frequency, na.rm = TRUE),
             q1_frequency = quantile(grid_all$frequency, 0.25, na.rm = TRUE),
             median_frequency = median(grid_all$frequency, na.rm = TRUE),
             q3_frequency = quantile(grid_all$frequency, 0.75, na.rm = TRUE),
             max_frequency = max(grid_all$frequency, na.rm = TRUE)) |>
      mutate(mean_score = mean(as.numeric(grid_all$score), na.rm = TRUE),
             sd_score = sd(as.numeric(grid_all$score), na.rm = TRUE),
             min_score = min(as.numeric(grid_all$score), na.rm = TRUE),
             q1_score = quantile(as.numeric(grid_all$score), 0.25, na.rm = TRUE),
             median_score = median(as.numeric(grid_all$score), na.rm = TRUE),
             q3_score = quantile(as.numeric(grid_all$score), 0.75, na.rm = TRUE),
             max_score = max(as.numeric(grid_all$score), na.rm = TRUE))
    
    if (!dir.exists("analysis")) {
      dir.create("analysis")
    }
    
    if (!file.exists("analysis/analysis_table.Rds")) {
      analysis_table = analysis_row # first run
    }
    else {
      analysis_table = readRDS("analysis/analysis_table.Rds")
      analysis_table = bind_rows(analysis_table, analysis_row)
    }
    
    saveRDS(analysis_table, "analysis/analysis_table.Rds")
    
    openxlsx::write.xlsx(analysis_table, file = "analysis/analysis_table.xlsx", overwrite = TRUE)
    
  }
  
}