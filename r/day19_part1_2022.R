
solve_blueprint <- function(bp) {
  cache <<- new.env(hash = TRUE, parent = emptyenv())
  global_best <<- 0
  
  max_needed <- c(
    max(bp$costs$ore[1], bp$costs$clay[1], bp$costs$obsidian[1], bp$costs$geode[1]),
    bp$costs$obsidian[2],
    bp$costs$geode[3]
  )

  dfs <- function(time, r, m) {
    if (time <= 0) return(m[4])

    optimistic <- m[4] + r[4] * time + (time * (time - 1)) %/% 2
    if (optimistic <= global_best) return(0)

    m_capped <- c(pmin(m[1:3], max_needed * time), m[4])
    key <- paste(time, paste(r, collapse = ","), paste(m_capped, collapse = ","), sep = "_")
    if (exists(key, envir = cache)) return(get(key, envir = cache))

    # Baseline: collect with current robots until time is up
    res <- m[4] + r[4] * time
    global_best <<- max(global_best, res)

    # Robot options: Geode, Obsidian, Clay, Ore
    for (i in 4:1) {
      if (i < 4 && r[i] >= max_needed[i]) next
      
      costs <- bp$costs[[i]]
      possible <- TRUE
      wait_time <- 0
      
      for (res_idx in 1:3) {
        cost_needed <- costs[res_idx]
        if (cost_needed > 0) {
          if (r[res_idx] == 0) {
            possible <- FALSE
            break
          }
          needed <- max(0, cost_needed - m[res_idx])
          wait_time <- max(wait_time, (needed + r[res_idx] - 1) %/% r[res_idx])
        }
      }
      
      if (possible) {
        total_wait <- wait_time + 1
        if (total_wait < time) {
          next_m <- m + r * total_wait
          next_m[1:3] <- next_m[1:3] - costs
          next_r <- r
          next_r[i] <- next_r[i] + 1
          
          res <- max(res, dfs(time - total_wait, next_r, next_m))
          global_best <<- max(global_best, res)
        }
      }
    }
    
    assign(key, res, envir = cache)
    return(res)
  }

  dfs(24, c(1, 0, 0, 0), c(0, 0, 0, 0))
}

lines <- readLines("input.txt", warn = FALSE)
total_quality <- 0

for (line in lines) {
  if (nchar(trimws(line)) == 0) next
  nums <- as.numeric(unlist(regmatches(line, gregexpr("[0-9]+", line))))
  
  bp <- list(
    id = nums[1],
    costs = list(
      ore      = c(nums[2], 0, 0),
      clay     = c(nums[3], 0, 0),
      obsidian = c(nums[4], nums[5], 0),
      geode    = c(nums[6], 0, nums[7])
    )
  )
  
  max_geodes <- solve_blueprint(bp)
  cat(sprintf("Blueprint %d: Max Geodes = %d\n", bp$id, max_geodes))
  total_quality <- total_quality + (bp$id * max_geodes)
}

cat("-----------------------------\n")
cat("Total Quality Level:", total_quality, "\n")
