#!/usr/bin/awk -f
BEGIN { main() }
function main() {
  count = 0
  fname = "input.txt"
  while ((getline line < fname) > 0) {
    pos = index(line, "|")
    if (pos > 0) {
      rest = substr(line, pos + 1)
      if (length(rest) > 0) {
        n = split(rest, toks, /[ \t]+/)
        for (i = 1; i <= n; i++) {
          tok = toks[i]
          sub(/\r$/, "", tok)
          L = length(tok)
          if (L == 2 || L == 3 || L == 4 || L == 7) count++
        }
      }
    }
  }
  close(fname)
  printf("%d\n", count)
}