
#!/usr/bin/env Rscript
# Knot Hash – Advent of Code 2017 Day 10 (Part Two)

knot_hash <- function(input) {
  # ---- 1. Build the length sequence ----
  lengths <- c(charToRaw(input), as.raw(c(17, 31, 73, 47, 23)))
  lengths <- as.integer(lengths)                 # raw -> int

  # ---- 2. Initialise state ----
  n <- 256L
  nums <- 0:(n - 1L)
  pos  <- 0L
  skip <- 0L

  # ---- 3. Run 64 rounds ----
  for (round in 1:64) {
    for (len in lengths) {
      if (len > 0L) {
        idx <- (pos + 0:(len - 1L)) %% n + 1L    # 1-based indexing
        nums[idx] <- rev(nums[idx])
      }
      pos  <- (pos + len + skip) %% n
      skip <- skip + 1L
    }
  }

  # ---- 4. Dense hash ----
  dense <- vapply(
    split(nums, (seq_along(nums) - 1L) %/% 16L),
    function(block) Reduce(bitwXor, block),
    integer(1)
  )

  # ---- 5. Hex string ----
  paste(sprintf("%02x", dense), collapse = "")
}

# ------------------------------------------------------------------
main <- function() {
  input <- paste(readLines("input.txt"), collapse = "")
  cat(knot_hash(input), "\n")
}

if (!interactive()) main()
