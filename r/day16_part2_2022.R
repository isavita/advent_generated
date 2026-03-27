
lines <- readLines("input.txt")
valves <- list()
for (line in lines) {
  id <- gsub("Valve ([A-Z]+) .*", "\\1", line)
  flow <- as.integer(gsub(".*rate=([0-9]+);.*", "\\1", line))
  targets <- gsub(".*valves? ", "", line)
  targets <- unlist(strsplit(targets, ", "))
  valves[[id]] <- list(flow = flow, targets = targets)
}

ids <- names(valves)
n <- length(ids)
dist <- matrix(1e6, nrow = n, ncol = n, dimnames = list(ids, ids))
diag(dist) <- 0
for (id in ids) {
  for (target in valves[[id]]$targets) dist[id, target] <- 1
}

for (k in ids) {
  for (i in ids) {
    for (j in ids) {
      if (dist[i, j] > dist[i, k] + dist[k, j]) {
        dist[i, j] <- dist[i, k] + dist[k, j]
      }
    }
  }
}

good_valves <- ids[sapply(valves, function(v) v$flow > 0)]
gv_flows <- sapply(good_valves, function(v) valves[[v]]$flow)
gv_n <- length(good_valves)
mask_map <- setNames(0:(gv_n - 1), good_valves)

max_p_for_mask <- numeric(2^gv_n)

dfs <- function(curr, time, mask, pressure) {
  max_p_for_mask[mask + 1] <<- max(max_p_for_mask[mask + 1], pressure)
  for (i in seq_along(good_valves)) {
    v <- good_valves[i]
    m_bit <- bitwShiftL(1, i - 1)
    if (bitwAnd(mask, m_bit) == 0) {
      rem_time <- time - dist[curr, v] - 1
      if (rem_time > 0) {
        dfs(v, rem_time, bitwOr(mask, m_bit), pressure + gv_flows[i] * rem_time)
      }
    }
  }
}

dfs("AA", 26, 0, 0)

ans <- 0
valid_masks <- which(max_p_for_mask > 0) - 1
pressures <- max_p_for_mask[valid_masks + 1]

# Optimization: Sort and prune if needed, but O(N^2) on reached masks is usually okay
for (i in seq_along(valid_masks)) {
  m1 <- valid_masks[i]
  p1 <- pressures[i]
  for (j in i:length(valid_masks)) {
    if (bitwAnd(m1, valid_masks[j]) == 0) {
      ans <- max(ans, p1 + pressures[j])
    }
  }
}

cat(ans, "\n")
