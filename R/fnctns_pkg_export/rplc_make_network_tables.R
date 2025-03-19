rplc_make_network_tables =  function(
    logger, wkt_object, links_pro, customer_name, network_table){


  table_network = createNetworkTable(
    customer_name = customer_name
    ,network_table = network_table
    ,links_pro = links_pro
    ,wkt_object = wkt_object )

  highway_counts = createNetworkLinkCountTable(
    customer_name = customer_name
    ,table_network = table_network)

  check_and_log_queired_links(
    counts_object = highway_counts
    ,query_links = links_pro
    ,logger_object = logger)

  return(
    list(
      table_network = table_network
      ,highway_counts = highway_counts
    )
  )
}
