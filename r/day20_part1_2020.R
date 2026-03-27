options(scipen = 999)
txt <- readLines("input.txt")
txt <- txt[txt != ""]
idx <- grep("Tile", txt)
ids <- as.numeric(gsub("\\D", "", txt[idx]))

sigs <- lapply(seq_along(idx), function(i) {
  r <- txt[(idx[i] + 1):(idx[i] + 10)]
  b <- c(r[1], r[10], 
         paste0(substr(r, 1, 1), collapse = ""), 
         paste0(substr(r, 10, 10), collapse = ""))
  sapply(b, function(x) {
    rv <- paste0(rev(strsplit(x, "")[[1]]), collapse = "")
    min(x, rv)
  })
})

boundary <- sapply(seq_along(sigs), function(i) {
  sum(!sigs[[i]] %in% unlist(sigs[-i]))
})

cat(format(prod(ids[boundary == 2]), scientific = FALSE), "\n")