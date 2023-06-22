#' Prompt User for Jitter Factor
#'
#' This function prompts the user to enter a jitter factor value between 0.00001
#' and 0.01. The function accepts the user's input and handles invalid input by
#' prompting the user again.
#'
#' @return A numeric value indicating the jitter factor entered by the user.
#'
#' @examples
#' prompt_jitter_factor()
#'
#' @export
#'
#' @keywords prompt, jitter factor, user input
#'
prompt_jitter_factor <- function() {
  while(TRUE) {
    jitter <- as.numeric(readline(prompt = "Provide jitter value: "))
    if(!is.na(jitter) && is.numeric(jitter) && jitter >= 0.00001 && jitter <= 0.01) {
      return(jitter)
    }
    else {
      cat("Invalid input. Jitter value must be a number between 0.00001 and 0.01.\n")
    }
  }
}


#' Merge Columns and Create Movement Descriptions
#'
#' This function takes a data frame as input and merges all columns whose names
#' start with "seq_" with a separate table called "table_network_data_simp".
#' For each merged column, it also creates a new column with a concatenated
#' string of the corresponding "streetName" values from the "table_network_data_simp"
#' table. Finally, the function creates two new columns in the input data frame:
#' "mvmnt_desc" and "mvmnt_desc_fl". The "mvmnt_desc" column contains a concatenated
#' string of all street names for each row, and the "mvmnt_desc_fl" column
#' contains a concatenated string of only the first and last street names for each row.
#'
#' @param data A data frame to be processed.
#'
#' @return A processed data frame with additional "mvmnt_desc" and "mvmnt_desc_fl"
#' columns.
#'
#' @examples
#' my_data <- data.frame(seq_1 = c(1,2,3), seq_2 = c(4,5,6), seq_3 = c(7,8,9),
#'                       stableEdgeId = c(1,2,3), streetName = c("First Street",
#'                       "Second Street", "Third Street"))
#' merge_cols(my_data)
#'
#' @export
#'
#' @import dplyr
#' @importFrom glue str_glue
#' @importFrom rlang parse_expr
#'
merge_cols <- function(data) {
  tmp_colnames = colnames(select(data, starts_with("seq_")))

  for (colname in tmp_colnames){
    data = merge(data
                 ,table_network_data_simp
                 ,by.x = colname, by.y = "stableEdgeId") %>%
      rename("{colname}_streetName" := "streetName")
  }

  tmp_colnames = colnames(select(data, ends_with("streetName")))

  tmp_func = paste0("{", tmp_colnames, "}", collapse = "_") %>%
    paste0("str_glue('", ., "')")

  tmp_func_fl = paste0("{", tmp_colnames[c(1,length(tmp_colnames))], "}", collapse = "_") %>%
    paste0("str_glue('", ., "')")

  data = data %>%
    mutate(mvmnt_desc = !!rlang::parse_expr(tmp_func)
           ,mvmnt_desc_fl = !!rlang::parse_expr(tmp_func_fl))

  return(data)
}

#' Bind columns with a specified prefix into a single data frame
#'
#' Given a data frame, this function binds together all columns with a specified
#' prefix, while maintaining their order, and returns the resulting data frame.
#'
#' @param data The input data frame.
#' @param prefix A character string representing the prefix of the columns to be bound.
#'               Default is "seq_".
#'
#' @return A data frame consisting of the columns with the specified prefix bound together.
#'
#' @examples
#' data <- data.frame(seq_1_link_id = 1, seq_1_streetName = "A",
#'                    seq_2_link_id = 2, seq_2_streetName = "B",
#'                    count = 5)
#' bind_cols_prefix(data)
#'
#' @export
bind_cols_prefix = function(data, prefix = "seq_"){

  tmp_colnames = colnames(select(data, starts_with(prefix))) %>%
    gsub("_link.*", "\\1", .) %>%
    unique() %>%
    sort()

  tmp_object = tmp_colnames %>%
    map_df(~{
      tmp = data %>%
        select(mode, starts_with("mvmnt_desc"), starts_with(.x), count)  %>%
        mutate(order = parse_number(.x))

      colnames(tmp) = colnames(tmp) %>%
        gsub(".*(streetName)", "\\1", .) %>%
        gsub(".*(link_id)", "\\1", .)

      return(tmp)
    })

  return(tmp_object)
}


# network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
# trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
# mode_type = c('PRIVATE_AUTO')
# customer_name = "replica-customer"
# jitter_factor = 0.003
# user_provided_save_location = "data/req_zz"





