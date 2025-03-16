
# Function to check if a room is real
is_real_room <- function(room_data) {
  parts <- unlist(strsplit(room_data, "\\["))
  checksum <- gsub("\\]", "", parts[2])
  name_sector <- unlist(strsplit(parts[1], "-"))
  sector_id <- as.integer(name_sector[length(name_sector)])
  name <- paste(name_sector[-length(name_sector)], collapse = "")
  
  freq <- table(strsplit(name, "")[[1]])
  sorted_freq <- sort(freq, decreasing = TRUE)
  top5 <- names(sorted_freq)
  
  # Handle ties by alphabetization
  if (length(top5) > 5) {
      counts <- as.vector(sorted_freq)
      i <- 5
      while (i > 1 && counts[i] == counts[i-1]) {
          i <- i - 1
      }
      if(i < 5) {
        top5_st <- top5[1:i]  #certain tops
        
        rem_freq <- sorted_freq[(i+1):length(sorted_freq)]
        rem_val <- counts[i]
        rem_tie <- names(rem_freq[rem_freq == rem_val])
        rem_tie_sort <- sort(rem_tie) # sort the ties
        
        top5 <- c(top5_st, rem_tie_sort[1:(5 - length(top5_st))]) #put them togehter
      }
      
      
  } else if (length(top5) < 5){
      
      top5 <- c(top5, rep('a',5-length(top5))) # should not really happen, but for completeness
      
  }
    
  generated_checksum <- paste(top5[1:5], collapse = "")
  
  return(list(is_real = generated_checksum == checksum, sector_id = sector_id))
}

# Function to decrypt room name
decrypt_name <- function(encrypted_name, sector_id) {
  decrypted_name <- ""
  for (char in strsplit(encrypted_name, "")[[1]]) {
    if (char == "-") {
      decrypted_name <- paste0(decrypted_name, " ")
    } else {
      decrypted_char_code <- (which(letters == char) - 1 + sector_id) %% 26 + 1
      decrypted_name <- paste0(decrypted_name, letters[decrypted_char_code])
    }
  }
  return(decrypted_name)
}

# Main function
main <- function() {
  # Read input from file
  input <- readLines("input.txt")
  
  total_sector_id_sum <- 0
  north_pole_sector_id <- 0
  
  for (room_data in input) {
    room_check <- is_real_room(room_data)
    if (room_check$is_real) {
      total_sector_id_sum <- total_sector_id_sum + room_check$sector_id
      
      # --- Part Two ---
      parts <- unlist(strsplit(room_data, "\\["))
      name_sector <- unlist(strsplit(parts[1], "-"))
      encrypted_name <- paste(name_sector[-length(name_sector)], collapse = "-")
      
      decrypted_name <- decrypt_name(encrypted_name, room_check$sector_id)
      if (grepl("north", decrypted_name, ignore.case = TRUE)  && grepl("pole", decrypted_name, ignore.case = TRUE))
            {
                north_pole_sector_id = room_check$sector_id
            }
    }
  }
  
  cat("Sum of sector IDs of real rooms:", total_sector_id_sum, "\n")
  cat("Sector ID of North Pole object storage:", north_pole_sector_id, "\n")
}

# Run the main function
main()
