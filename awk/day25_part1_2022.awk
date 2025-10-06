BEGIN {
  sum = 0
  while ((getline s < "input.txt") > 0) {
    sum += fromSnafu(s)
  }
  close("input.txt")
  print toSnafu(sum)
  exit
}
function fromSnafu(s, n, i, ch) {
  n = 0
  for (i = 1; i <= length(s); i++) {
    n *= 5
    ch = substr(s, i, 1)
    if (ch == "=") {
      n -= 2
    } else if (ch == "-") {
      n -= 1
    } else {
      n += ch + 0
    }
  }
  return n
}
function toSnafu(n, out, rem, i, rev) {
  if (n == 0) return "0"
  out = ""
  while (n > 0) {
    rem = n % 5
    if (rem == 3) {
      n += 5
      out = out "="
    } else if (rem == 4) {
      n += 5
      out = out "-"
    } else {
      out = out rem
    }
    n = int(n / 5)
  }
  rev = ""
  for (i = length(out); i >= 1; i--) {
    rev = rev substr(out, i, 1)
  }
  return rev
}