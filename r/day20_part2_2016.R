# Read the input file
blocked_ranges <- readLines("input.txt")

# Parse the ranges into a list of numeric vectors
ranges <- do.call(rbind, lapply(blocked_ranges, function(x) as.numeric(unlist(strsplit(x, "-")))))

# Sort ranges by starting value
ranges <- ranges[order(ranges[, 1]), ]

# Initialize variables
allowed_ips <- 0
lowest_unblocked_ip <- 0
current_max <- 0

# Iterate through the sorted ranges
for (range in 1:nrow(ranges)) {
  start <- ranges[range, 1]
  end <- ranges[range, 2]
  
  if (current_max < start) {
    allowed_ips <- allowed_ips + (start - current_max - 1)
    if (lowest_unblocked_ip == 0) {
      lowest_unblocked_ip <- current_max + 1
    }
  }
  
  current_max <- max(current_max, end)
}

# Check for allowed IPs after the last range
if (current_max < 4294967295) {
  allowed_ips <- allowed_ips + (4294967295 - current_max)
}

# Print results
cat("Lowest unblocked IP:", lowest_unblocked_ip, "\n")
cat("Total allowed IPs:", allowed_ips, "\n")