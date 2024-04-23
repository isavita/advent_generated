is_prime <- function(n) {
  if (n <= 1) return(FALSE)
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) return(FALSE)
  }
  TRUE
}

b <- 57*100 + 100000
c <- b + 17000
h <- 0

for (x in seq(b, c, by = 17)) {
  if (!is_prime(x)) h <- h + 1
}

print(h)