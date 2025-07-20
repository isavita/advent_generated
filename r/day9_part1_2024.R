
main <- function() {
  txt <- readLines("input.txt", n = 1L)
  if (is.na(txt) || txt == "") {
    cat("0\n")
    return()
  }
  
  # Build disk in one pass
  disk <- integer()
  file_id <- 0L
  for (i in seq_along(strsplit(txt, "")[[1]])) {
    len <- as.integer(substr(txt, i, i))
    if (i %% 2 == 1) {
      disk <- c(disk, rep(file_id, len))
      file_id <- file_id + 1L
    } else {
      disk <- c(disk, rep(-1L, len))
    }
  }
  
  # Compact
  left  <- 1L
  right <- length(disk)
  while (left < right) {
    if (disk[left] != -1L) {
      left <- left + 1L
      next
    }
    if (disk[right] == -1L) {
      right <- right - 1L
      next
    }
    disk[left]  <- disk[right]
    disk[right] <- -1L
    left  <- left  + 1L
    right <- right - 1L
  }
  
  # Checksum
  pos <- which(disk != -1L) - 1L
  checksum <- sum(pos * disk[disk != -1L])
  cat(sprintf("%s\n", as.character(checksum)))
}

main()
