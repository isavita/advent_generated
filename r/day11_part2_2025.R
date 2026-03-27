
options(scipen = 999)
lines <- readLines("input.txt", warn = FALSE)
adj <- list()

for (line in lines) {
  if (grepl(":", line)) {
    parts <- strsplit(line, ":")[[1]]
    src <- trimws(parts[1])
    dests <- strsplit(trimws(parts[2]), "\\s+")[[1]]
    adj[[src]] <- dests
  }
}

memo <- new.env()

count_paths <- function(u, target) {
  if (u == target) return(1)
  key <- paste(u, target, sep = "|")
  if (exists(key, envir = memo)) return(get(key, envir = memo))
  
  res <- 0
  if (!is.null(adj[[u]])) {
    for (v in adj[[u]]) {
      res <- res + count_paths(v, target)
    }
  }
  
  assign(key, res, envir = memo)
  return(res)
}

s1 <- count_paths("svr", "dac") * count_paths("dac", "fft") * count_paths("fft", "out")
s2 <- count_paths("svr", "fft") * count_paths("fft", "dac") * count_paths("dac", "out")

cat(sprintf("Paths (svr->dac->fft->out): %.0f\n", s1))
cat(sprintf("Paths (svr->fft->dac->out): %.0f\n", s2))
cat(sprintf("Total paths visiting both: %.0f\n", s1 + s2))
