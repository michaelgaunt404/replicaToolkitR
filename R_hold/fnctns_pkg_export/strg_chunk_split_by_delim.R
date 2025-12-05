strg_chunk_split_by_delim <- function(input_string, max_length = 800000, delimiter = ",") {
  # Check if the input string exceeds the max length
  if (nchar(input_string) <= max_length) {
    return(list(input_string))  # No need to split if it's within the limit
  }

  # Split the input string by the delimiter and remove extra spaces
  # input_parts <- unlist(strsplit(input_string, paste0("\\s*", delimiter, "\\s*")))

  i = 1
  temp_index = list()
  max_iterations = ceiling(nchar(input_string)/max_length)
  offset = 0
  for (i in 1:max_iterations){
    front = (1+max_length*(i-1)) + offset
    back = (max_length*(i)) + offset

    message(str_glue("Iteration: {i}/{max_iterations}"))


    chunk_prime = substring(input_string, front, back)
    str_trunc(chunk_prime, width = 60, "right")
    str_trunc(chunk_prime, width = 60, "left")
    check_empty = is.na(chunk_prime)
    check_last = str_sub(chunk_prime,-1,-1)


    if (check_empty){
      temp_index[[i]] = NA
      offset_new = 0

    } else {

      if (check_last=="'"){
        temp_index[[i]] = chunk_prime
        offset_new = 0
      } else {
        next_window = substring(input_string, back+1, back+100)

        index_next_location =  next_window %>%
          str_locate(., ",") %>%
          .[[1]]

        chunk_alt = substring(input_string, front, back+index_next_location) %>%
          str_remove(., ",$") %>%
          str_trim()

        temp_index[[i]] = chunk_alt
        offset_new = index_next_location
        message(str_glue("Offset : {offset}"))
      }
    }
    offset = offset_new + offset

  }

  result = temp_index[!is.na(temp_index)]

  return(result)
}


