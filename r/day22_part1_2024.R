
s <- as.numeric(readLines("input.txt", warn = FALSE))
s <- s[!is.na(s)]
for (i in 1:2000) {
  s <- bitwXor(s %% 16777216, (s * 64) %% 16777216) %% 16777216
  s <- bitwXor(s, floor(s / 32)) %% 16777216
  s <- bitwXor(s, (s * 2048) %% 16777216) %% 16777216
}
cat(format(sum(s), scientific = FALSE), "\n")
