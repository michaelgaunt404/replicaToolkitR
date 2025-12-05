rplc_split_long_string <- function(x, max_len = 500000) {
  stopifnot(is.character(x), length(x) == 1)

  # Split into individual elements
  items <- strsplit(x, ",", fixed = TRUE)[[1]]
  output <- list()

  current_chunk <- character()
  current_length <- 0

  for (item in items) {
    item_len <- nchar(item)
    # +1 for comma (unless first element)
    extra_len <- if (length(current_chunk) == 0) item_len else item_len + 1

    if (current_length + extra_len > max_len) {
      # Commit current chunk
      output[[length(output) + 1]] <- paste(current_chunk, collapse = ",")
      # Start new chunk
      current_chunk <- item
      current_length <- item_len
    } else {
      current_chunk <- c(current_chunk, item)
      current_length <- current_length + extra_len
    }
  }

  # Add final chunk
  if (length(current_chunk) > 0) {
    output[[length(output) + 1]] <- paste(current_chunk, collapse = ",")
  }

  return(output)
}
