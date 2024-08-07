#' Aggregate and summarize link data based on count.
#'
#' This function generates a BigQuery query to aggregate and summarize link data
#' based on the count of records for each link. It categorizes links into count
#' ranges and provides the count for each category.
#'
#' @param customer_name The name of the BigQuery customer project.
#' @param table_agg_by_link_subset The name of the table containing link data.
#'
#' @return A BigQuery result containing the count of links in different categories.
#'
#' @examples
#' queryAggByLinkSumSubset("your_project_name", "your_agg_by_link_subset_table")
#'
#' @export
createAggByLinkSumSubsetTable <- function(customer_name, table_agg_by_link_subset) {

  message(stringr::str_glue("{make_space()}\nCounting number of links by their total volume..."))

  query <- str_glue("select flag_link, count(*) as count
    from (
    select *,
    case
    when count = 1 then '1 count'
    when count = 2 then '2 count'
    when count = 3 then '3 count'
    when count = 4 then '4 count'
    when count = 5 then '5 count'
    when count <= 10 then '6-10 count'
    else '11 or greater' end as flag_link
    from {replica_temp_tbl_name(table_agg_by_link_subset)}
    ) group by flag_link
    order by count")

  table_agg_by_link_sum_subset <- bigrquery::bq_project_query(customer_name, query)

  table_dl = bigrquery::bq_table_download(table_agg_by_link_sum_subset)

  table_dl_pro = table_dl %>%
    mutate(flag_link = forcats::fct_relevel(flag_link,
                                            c("1 count", "2 count", "3 count"
                                              ,"4 count", '5 count', "6-10 count", "11 or greater"))) %>%
    arrange(flag_link) %>%
    mutate(percent = 100*gauntlet::dgt3(count/sum(count))
           ,count_cum = cumsum(count)
           ,percent_cum = 100*gauntlet::dgt3(count_cum/sum(count))) %>%
    arrange(desc(flag_link)) %>%
    mutate(count_rm = cumsum(count)
           ,percent_rm = cumsum(percent))

  message(stringr::str_glue("Completed{make_space()}"))

  return(table_dl_pro)
}
