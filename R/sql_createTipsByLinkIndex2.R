sql_createTipsByLinkIndex2 = function(customer_name, trip_table, mode_type_pro, link_selections_index_pro) {
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}\nFiltering for trips that use specified links...."))

  query <- stringr::str_glue("select
*
,ROW_NUMBER () OVER (PARTITION BY activity_id) AS link_ord from (
select * from (
select * except(network_link_ids, geometry, transit_route_ids)
from (
select *
from `{trip_table}`
where 1=1
and mode in ({mode_type_pro})
) as foo, unnest(network_link_ids) as network_links) as foo3
where 1=1
and network_links in ({link_selections_index_pro})
) as foo4;")

  tmp_object <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{gauntlet::strg_make_space_2()}"))

  return(tmp_object)
}
