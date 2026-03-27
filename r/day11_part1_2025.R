
lines <- readLines("input.txt", warn = FALSE)
adj <- list()
for (line in lines) {
  parts <- strsplit(line, ":")[[1]]
  u <- trimws(parts[1])
  v_list <- strsplit(trimws(parts[2]), " ")[[1]]
  adj[[u]] <- v_list[v_list != ""]
}

memo <- new.env(parent = emptyenv())

dfs <- function(u, target) {
  if (u == target) return(1)
  if (exists(u, envir = memo)) return(memo[[u]])
  
  total <- 0
  if (!is.null(adj[[u]])) {
    for (v in adj[[u]]) {
      total <- total + dfs(v, target)
    }
  }
  
  memo[[u]] <- total
  return(total)
}

cat(dfs("you", "out"), "\n")
