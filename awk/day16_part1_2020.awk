BEGIN {
  fname = "input.txt"
  scanning = 1
  numRules = 0
  errorRate = 0
  while ((getline line < fname) > 0) {
    processLine(line)
  }
  exit
}
function processLine(line,   endpos, trim, rangePart, parts, a, b, rl1, rh1, rl2, rh2, n, i, nums, val, ok, k) {
  if (scanning) {
    if (line ~ /^$/) return
    if (line ~ /(your ticket:|nearby tickets:)/) { scanning = 0; return }
    if (index(line, ":") > 0) {
      endpos = index(line, ":")
      rangePart = substr(line, endpos + 1)
      trim = rangePart
      gsub(/^[ \t]+|[ \t]+$/, "", trim)
      split(trim, parts, " or ")
      split(parts[1], a, "-"); rl1 = a[1] + 0; rh1 = a[2] + 0
      split(parts[2], b, "-"); rl2 = b[1] + 0; rh2 = b[2] + 0
      numRules++
      r1_lo[numRules] = rl1
      r1_hi[numRules] = rh1
      r2_lo[numRules] = rl2
      r2_hi[numRules] = rh2
    }
  } else {
    if (line ~ /(your ticket:|nearby tickets:)/) { return }
    trimLine = line
    gsub(/^[ \t]+|[ \t]+$/, "", trimLine)
    if (trimLine == "") return
    n = split(trimLine, nums, ",")
    for (i = 1; i <= n; i++) {
      val = nums[i] + 0
      ok = 0
      for (k = 1; k <= numRules; k++) {
        if ((val >= r1_lo[k] && val <= r1_hi[k]) || (val >= r2_lo[k] && val <= r2_hi[k])) { ok = 1; break }
      }
      if (!ok) errorRate += val
    }
  }
}
END { print errorRate }