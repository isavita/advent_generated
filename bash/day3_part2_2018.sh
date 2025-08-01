#!/usr/bin/env bash
# Reads input from input.txt and prints:
# Part 1: number of square inches within two or more claims
# Part 2: ID of the single claim that does not overlap

set -euo pipefail

main() {
  local file="input.txt"
  [[ -f "$file" ]] || { echo "input.txt not found" >&2; exit 1; }

  # We map each covered cell to a count using an awk associative array.
  # Also store each claim's cells to later check for overlaps efficiently.
  awk '
  function split_cells(cells, arr,   n,i) {
    n = split(cells, arr, " ")
    # trim potential empty due to leading space
    if (arr[1] == "") {
      for (i=1;i<n;i++) arr[i]=arr[i+1]
      n--
    }
    return n
  }
  BEGIN {
    FS = "[# @,:x]+"    # parse: #ID @ left,top: widthxheight
  }
  {
    # Fields after split: $2=id, $3=left, $4=top, $5=width, $6=height
    id = $2; left = $3; top = $4; w = $5; h = $6
    claim_ids[++N] = id
    # enumerate covered cells; encode as "x,y"
    cells = ""
    for (y = top; y < top + h; y++) {
      for (x = left; x < left + w; x++) {
        key = x "," y
        counts[key]++
        cells = cells " " key
      }
    }
    claim_cells[id] = cells
  }
  END {
    # Part 1: count cells with count >= 2
    overlap = 0
    for (k in counts) if (counts[k] >= 2) overlap++

    # Part 2: find claim whose every cell has count == 1
    intact = ""
    for (i=1; i<=N; i++) {
      id = claim_ids[i]
      cells = claim_cells[id]
      n = split_cells(cells, tmp)
      ok = 1
      for (j=1; j<=n; j++) {
        if (counts[tmp[j]] != 1) { ok = 0; break }
      }
      if (ok) { intact = id; break }
    }

    print overlap
    print intact
  }' "$file"
}

main "$@"