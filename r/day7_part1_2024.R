
lines <- readLines("input.txt", warn = FALSE)
total_sum <- 0

can_match <- function(target, current, nums) {
  if (length(nums) == 0) return(current == target)
  if (current > target) return(FALSE)
  
  next_val <- nums[1]
  remaining <- nums[-1]
  
  return(can_match(target, current + next_val, remaining) || 
         can_match(target, current * next_val, remaining))
}

for (line in lines) {
  if (line == "") next
  parts <- strsplit(line, ": ")[[1]]
  target <- as.numeric(parts[1])
  nums <- as.numeric(strsplit(parts[2], " ")[[1]])
  
  if (can_match(target, nums[1], nums[-1])) {
    total_sum <- total_sum + target
  }
}

cat(format(total_sum, scientific = FALSE), "\n")
