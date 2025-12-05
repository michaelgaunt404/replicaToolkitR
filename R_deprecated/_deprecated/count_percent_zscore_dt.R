#deprecated
# count_percent_zscore_dt = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
#                                    col, prefix = NULL, rnd = NULL, cntr_scl = FALSE){
#
#   tmp = data %>%
#     data.table::data.table() %>%
#     .[,.(count = sum(.SD)), .SDcols = col, by = grp_c] %>%
#     .[,`:=`(percent = (count/sum(count)) %>%
#               { if (!is.null(rnd)) round(., rnd) else .}), by = grp_p] %>%
#     { if (cntr_scl) (.) %>%
#         .[,`:=`(zscore = as.vector(scale(count))), by = grp_z]
#       else .}
#
#   if (is.null(prefix)){
#     tmp = tmp
#   } else {
#     newname1 = str_glue("{prefix}_count")
#     newname2 = str_glue("{prefix}_percent")
#     rename(tmp, !!newname1 := count, !!newname2 := percent)
#   }
#
#   return(tmp)
# }
