replica_test_connection = function(network_table = network_table
                                   ,trip_table = trip_table
                                   ,customer_name = customer_name){

  #NOTE: I think that this is now captured in checkValidTableConnections which is in the replica package
  #this script does need a replica_test_connection

  test_outcome = list(
    c(network_table, trip_table)
    ,c("distance", "activity_id")
  ) %>%
    pmap(~{

      outcome = tryCatch({

        table = bigrquery::bq_project_query(
          customer_name, stringr::str_glue("select {.y} from `{.x}` limit 1;"))

        table_dl = bigrquery::bq_table_download(table)

        return(nrow(table_dl))
      },  error = function(e) {
        error_message <- paste("An error occurred while querying the trip table:\n", e$message)

        return(NA)
      })

    })

  stopifnot("One or more of the Replica tables could not be connected to.... \nCheck Replica table inputs" = any(is.na(unlist(test_outcome))) == F)
}
