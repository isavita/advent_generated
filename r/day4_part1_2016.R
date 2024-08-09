sum_of_sector_ids <- function() {
  lines <- readLines("input.txt")
  is_real_room <- function(room) {
    parts <- strsplit(room, "\\[")[[1]]
    checksum <- gsub("\\]", "", parts[2])
    encrypted_name <- unlist(strsplit(parts[1], "-"))[-length(unlist(strsplit(parts[1], "-")))]
    letter_counts <- table(unlist(strsplit(paste(encrypted_name, collapse = ""), "")))
    counts <- sort(letter_counts, decreasing = TRUE)
    sorted_letters <- names(counts)
    sorted_counts <- as.vector(counts)
    
    result <- ""
    for (i in seq_along(sorted_letters)) {
      result <- paste0(result, sorted_letters[i])
      if (nchar(result) == nchar(checksum)) break
    }
    
    return(substr(result, 1, nchar(checksum)) == checksum)
  }
  
  get_sector_id <- function(room) {
    as.numeric(sub(".*-(\\d+)\\[.*", "\\1", room))
  }
  
  sum_of_ids <- sum(sapply(lines, function(line) {
    if (is_real_room(line)) get_sector_id(line) else 0
  }))
  
  print(sum_of_ids)
}

sum_of_sector_ids()