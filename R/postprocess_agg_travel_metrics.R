
#' Post-process aggregated travel metrics
#'
#' Convenience helper that summarizes trip-level travel metrics into
#' counts, selected quantiles, and means for duration, distance, and speed.
#'
#' @param dt A data.table containing trip-level data with
#'   `duration_minutes`, `distance_miles`, and `speed` columns.
#' @param index_agg Optional grouping variable(s) passed to `by`
#'   (e.g., c(''var_1', 'var_2')).
#'
#' @importFrom DescTools Quantile
#'
#' @return A data.table with aggregated travel metrics by `index_agg`.
#'
#' @export
postprocess_agg_travel_metrics = function(dt, index_agg = NULL){
  dt %>%
    .[,.(
      count = .N
      ,trp_dur_p05 = as.numeric(DescTools::Quantile(duration_minutes, probs = .05, na.rm = T)) %>% round(2)
      ,trp_dur_p15 = as.numeric(DescTools::Quantile(duration_minutes, probs = .15, na.rm = T)) %>% round(2)
      ,trp_dur_p50 = as.numeric(DescTools::Quantile(duration_minutes, probs = .5, na.rm = T)) %>% round(2)
      ,trp_dur_p85 = as.numeric(DescTools::Quantile(duration_minutes, probs = .85, na.rm = T)) %>% round(2)
      ,trp_dur_p95 = as.numeric(DescTools::Quantile(duration_minutes, probs = .95, na.rm = T)) %>% round(2)
      ,trp_dur_mean = as.numeric(mean(duration_minutes, na.rm = T)) %>% round(2)
      ,trp_dis_p05 = as.numeric(DescTools::Quantile(distance_miles, probs = .05, na.rm = T)) %>% round(2)
      ,trp_dis_p15 = as.numeric(DescTools::Quantile(distance_miles, probs = .15, na.rm = T)) %>% round(2)
      ,trp_dis_p50 = as.numeric(DescTools::Quantile(distance_miles, probs = .5, na.rm = T)) %>% round(2)
      ,trp_dis_p85 = as.numeric(DescTools::Quantile(distance_miles, probs = .85, na.rm = T)) %>% round(2)
      ,trp_dis_p95 = as.numeric(DescTools::Quantile(distance_miles, probs = .95, na.rm = T)) %>% round(2)
      ,trp_dis_mean = as.numeric(mean(distance_miles, na.rm = T)) %>% round(2)
      ,trp_sp_p05 = as.numeric(DescTools::Quantile(speed, probs = .05, na.rm = T)) %>% round(2)
      ,trp_sp_p15 = as.numeric(DescTools::Quantile(speed, probs = .15, na.rm = T)) %>% round(2)
      ,trp_sp_p50 = as.numeric(DescTools::Quantile(speed, probs = .5, na.rm = T)) %>% round(2)
      ,trp_sp_p85 = as.numeric(DescTools::Quantile(speed, probs = .85, na.rm = T)) %>% round(2)
      ,trp_sp_p95 = as.numeric(DescTools::Quantile(speed, probs = .95, na.rm = T)) %>% round(2)
      ,trp_sp_mean = as.numeric(mean(speed, na.rm = T)) %>% round(2))
      ,by = index_agg
    ]
}
