options(scipen = 999)

can_match <- function(target, current, nums, idx, n) {
  if (idx > n) return(current == target)
  if (current > target) return(FALSE)
  
  nv <- nums[idx]
  
  if (can_match(target, current + nv, nums, idx + 1, n)) return(TRUE)
  
  if (can_match(target, current * nv, nums, idx + 1, n)) return(TRUE)
  
  m <- 10^floor(log10(nv + 1e-10) + 1)
  if (nv == 0) m <- 10
  if (can_match(target, current * m + nv, nums, idx + 1, n)) return(TRUE)
  
  FALSE
}

solve <- function() {
  if (!file.exists("input.txt")) return()
  lines <- readLines("input.txt", warn = FALSE)
  total_sum <- 0
  
  for (line in lines) {
    if (nchar(trimws(line)) == 0) next
    parts <- strsplit(line, ":")[[1]]
    if (length(parts) < 2) next
    
    target <- as.numeric(parts[1])
    nums <- as.numeric(strsplit(trimws(parts[2]), "\\s+")[[1]])
    
    if (length(nums) > 0) {
      if (can_match(target, nums[1], nums, 2, length(nums))) {
        total_sum <- total_sum + target
      }
    }
  }
  
  cat(format(total_sum, scientific = FALSE), "\n")
}

solve()