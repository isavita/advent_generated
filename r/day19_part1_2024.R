
solve <- function() {
  con <- file("input.txt", "r")
  on.exit(close(con))
  
  available_line <- readLines(con, n = 1)
  available_patterns <- trimws(strsplit(available_line, ",")[[1]])
  
  readLines(con, n = 1)
  
  count <- 0
  while(TRUE) {
    design <- readLines(con, n = 1)
    if(length(design) == 0) break
    if(can_make(design, available_patterns)) {
      count <- count + 1
    }
  }
  
  cat(count, "\n")
}

can_make <- function(design, patterns) {
  n <- nchar(design)
  dp <- rep(FALSE, n + 1)
  dp[1] <- TRUE
  
  for (i in 1:n) {
    if(dp[i]){
      for (p in patterns) {
        lp <- nchar(p)
        if (i + lp <= n + 1 && substr(design, i, i + lp - 1) == p) {
          dp[i + lp] <- TRUE
        }
      }
    }
  }
  return(dp[n + 1])
}

solve()
