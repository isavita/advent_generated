
options(scipen = 999)
input <- readLines("input.txt", warn = FALSE)
stones <- unlist(strsplit(input, "\\s+"))
stones <- stones[stones != ""]
counts <- table(stones)
v <- names(counts)
n <- as.numeric(counts)

for (i in 1:75) {
  is_z <- v == "0"
  nc <- nchar(v)
  is_e <- !is_z & (nc %% 2 == 0)
  is_o <- !is_z & !is_e
  
  ve <- v[is_e]
  ne <- n[is_e]
  mid <- nc[is_e] / 2
  
  nxt_v <- c(rep("1", sum(is_z)), 
             as.character(as.numeric(substr(ve, 1, mid))),
             as.character(as.numeric(substr(ve, mid + 1, nc[is_e]))),
             as.character(as.numeric(v[is_o]) * 2024))
  nxt_n <- c(n[is_z], ne, ne, n[is_o])
  
  agg <- rowsum(nxt_n, nxt_v)
  v <- rownames(agg)
  n <- as.vector(agg)
}

cat(format(sum(n), scientific = FALSE), "\n")
