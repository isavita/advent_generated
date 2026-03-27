options(scipen = 999)
prog <- as.numeric(scan("input.txt", sep = ",", quiet = TRUE))

get_m <- function(e, a) {
  i <- a + 1
  if (i > length(e$mem)) return(0)
  v <- e$mem[i]
  if (is.na(v)) 0 else v
}
set_m <- function(e, a, v) { e$mem[a + 1] <- v }
get_p <- function(e, m, o) {
  v <- get_m(e, e$ip + o)
  if (m == 0) get_m(e, v) else if (m == 1) v else if (m == 2) get_m(e, e$rb + v) else 0
}
get_a <- function(e, m, o) {
  v <- get_m(e, e$ip + o)
  if (m == 0) v else if (m == 2) e$rb + v else 0
}

run_comp <- function(e) {
  while (!e$halted) {
    ins <- get_m(e, e$ip)
    op <- ins %% 100
    m1 <- (ins %/% 100) %% 10; m2 <- (ins %/% 1000) %% 10; m3 <- (ins %/% 10000) %% 10
    if (op == 3) {
      if (length(e$inputs) == 0) { e$idle <- TRUE; return() }
      val <- e$inputs[1]; e$inputs <- e$inputs[-1]
      set_m(e, get_a(e, m1, 1), val)
      e$ip <- e$ip + 2
      if (val == -1) { e$idle <- TRUE; return() }
      e$idle <- FALSE
    } else if (op == 4) {
      e$outputs <- c(e$outputs, get_p(e, m1, 1))
      e$ip <- e$ip + 2; e$idle <- FALSE
      if (length(e$outputs) >= 3) return()
    } else if (op == 99) {
      e$halted <- TRUE
    } else {
      e$idle <- FALSE
      if (op == 1) { set_m(e, get_a(e, m3, 3), get_p(e, m1, 1) + get_p(e, m2, 2)); e$ip <- e$ip + 4 }
      else if (op == 2) { set_m(e, get_a(e, m3, 3), get_p(e, m1, 1) * get_p(e, m2, 2)); e$ip <- e$ip + 4 }
      else if (op == 5) { if (get_p(e, m1, 1) != 0) e$ip <- get_p(e, m2, 2) else e$ip <- e$ip + 3 }
      else if (op == 6) { if (get_p(e, m1, 1) == 0) e$ip <- get_p(e, m2, 2) else e$ip <- e$ip + 3 }
      else if (op == 7) { set_m(e, get_a(e, m3, 3), if (get_p(e, m1, 1) < get_p(e, m2, 2)) 1 else 0); e$ip <- e$ip + 4 }
      else if (op == 8) { set_m(e, get_a(e, m3, 3), if (get_p(e, m1, 1) == get_p(e, m2, 2)) 1 else 0); e$ip <- e$ip + 4 }
      else if (op == 9) { e$rb <- e$rb + get_p(e, m1, 1); e$ip <- e$ip + 2 }
    }
  }
}

computers <- lapply(0:49, function(id) {
  e <- new.env(parent = emptyenv())
  e$mem <- prog; e$ip <- 0; e$rb <- 0; e$inputs <- as.numeric(id)
  e$outputs <- numeric(); e$halted <- FALSE; e$idle <- FALSE
  e
})

packet_queues <- replicate(50, numeric(), simplify = FALSE)
nat_x <- -1; nat_y <- -1; prev_nat_y <- -2; nat_has_packet <- FALSE

while (TRUE) {
  network_was_idle <- TRUE
  for (i in 1:50) {
    e <- computers[[i]]
    if (e$halted) next
    if (length(packet_queues[[i]]) > 0) {
      network_was_idle <- FALSE
      e$inputs <- c(e$inputs, packet_queues[[i]])
      packet_queues[[i]] <- numeric()
    }
    if (length(e$inputs) == 0) e$inputs <- -1
    run_comp(e)
    if (!e$idle) network_was_idle <- FALSE
    while (length(e$outputs) >= 3) {
      network_was_idle <- FALSE
      dest <- e$outputs[1]; x <- e$outputs[2]; y <- e$outputs[3]
      e$outputs <- e$outputs[-(1:3)]
      if (dest == 255) { nat_x <- x; nat_y <- y; nat_has_packet <- TRUE }
      else if (dest >= 0 && dest < 50) packet_queues[[dest + 1]] <- c(packet_queues[[dest + 1]], x, y)
    }
  }
  if (network_was_idle && all(sapply(packet_queues, length) == 0)) {
    if (nat_has_packet) {
      if (nat_y == prev_nat_y) {
        cat(sprintf("%.0f\n", nat_y))
        break
      }
      prev_nat_y <- nat_y
      packet_queues[[1]] <- c(nat_x, nat_y)
      nat_has_packet <- FALSE
    }
  }
}