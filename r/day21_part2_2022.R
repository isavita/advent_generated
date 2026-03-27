
monkeys <- new.env()
for (l in readLines("input.txt", warn = FALSE)) {
  s <- strsplit(l, ": ")[[1]]
  monkeys[[s[1]]] <- strsplit(s[2], " ")[[1]]
}

memo <- new.env()
calc <- function(n) {
  if (n == "humn") return(NA)
  if (exists(n, memo)) return(memo[[n]])
  m <- monkeys[[n]]
  if (length(m) == 1) return(memo[[n]] <- as.numeric(m))
  a <- calc(m[1]); b <- calc(m[3])
  if (is.na(a) || is.na(b)) return(NA)
  memo[[n]] <- switch(m[2], "+" = a + b, "-" = a - b, "*" = a * b, "/" = a / b)
}

find <- function(n, target) {
  if (n == "humn") return(target)
  m <- monkeys[[n]]
  a <- calc(m[1])
  if (is.na(a)) {
    b <- calc(m[3])
    v <- switch(m[2], "+" = target - b, "-" = target + b, "*" = target / b, "/" = target * b, "=" = b)
    find(m[1], v)
  } else {
    v <- switch(m[2], "+" = target - a, "-" = a - target, "*" = target / a, "/" = a / target, "=" = a)
    find(m[3], v)
  }
}

monkeys[["root"]][2] <- "="
cat(sprintf("%.0f\n", find("root", 0)))
