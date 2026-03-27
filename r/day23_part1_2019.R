options(scipen = 999)
input_str <- readLines("input.txt", warn = FALSE)
prog <- as.numeric(strsplit(input_str, ",")[[1]])
mem_template <- rep(0, 10000)
mem_template[1:length(prog)] <- prog
comps <- lapply(0:49, function(id) {
  e <- new.env()
  e$mem <- mem_template
  e$ip <- 0
  e$rb <- 0
  e$inputs <- as.numeric(id)
  e$outputs <- numeric()
  e
})
while (TRUE) {
  for (i in 1:50) {
    c <- comps[[i]]
    mem <- c$mem; ip <- c$ip; rb <- c$rb; inputs <- c$inputs
    while (TRUE) {
      ins <- mem[ip + 1]
      op <- ins %% 100
      m1 <- (ins %/% 100) %% 10
      m2 <- (ins %/% 1000) %% 10
      m3 <- (ins %/% 10000) %% 10
      if (op == 1) {
        v1 <- if (m1 == 1) mem[ip+2] else { idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        v2 <- if (m2 == 1) mem[ip+3] else { idx <- if (m2 == 0) mem[ip+3]+1 else rb+mem[ip+3]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        p3 <- if (m3 == 0) mem[ip+4]+1 else rb+mem[ip+4]+1
        mem[p3] <- v1 + v2; ip <- ip + 4
      } else if (op == 2) {
        v1 <- if (m1 == 1) mem[ip+2] else { idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        v2 <- if (m2 == 1) mem[ip+3] else { idx <- if (m2 == 0) mem[ip+3]+1 else rb+mem[ip+3]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        p3 <- if (m3 == 0) mem[ip+4]+1 else rb+mem[ip+4]+1
        mem[p3] <- v1 * v2; ip <- ip + 4
      } else if (op == 3) {
        idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1
        if (length(inputs) == 0) {
          mem[idx] <- -1; ip <- ip + 2; c$mem <- mem; c$ip <- ip; c$rb <- rb; c$inputs <- inputs; break
        } else {
          mem[idx] <- inputs[1]; inputs <- inputs[-1]; ip <- ip + 2
        }
      } else if (op == 4) {
        v1 <- if (m1 == 1) mem[ip+2] else { idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        c$outputs <- c(c$outputs, v1); ip <- ip + 2
        if (length(c$outputs) == 3) {
          dst <- c$outputs[1]; x <- c$outputs[2]; y <- c$outputs[3]; c$outputs <- numeric()
          if (dst == 255) { cat(sprintf("%.0f\n", y)); quit(save = "no") }
          if (dst >= 0 && dst < 50) comps[[dst+1]]$inputs <- c(comps[[dst+1]]$inputs, x, y)
          c$mem <- mem; c$ip <- ip; c$rb <- rb; c$inputs <- inputs; break
        }
      } else if (op == 5) {
        v1 <- if (m1 == 1) mem[ip+2] else { idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        v2 <- if (m2 == 1) mem[ip+3] else { idx <- if (m2 == 0) mem[ip+3]+1 else rb+mem[ip+3]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        if (v1 != 0) ip <- v2 else ip <- ip + 3
      } else if (op == 6) {
        v1 <- if (m1 == 1) mem[ip+2] else { idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        v2 <- if (m2 == 1) mem[ip+3] else { idx <- if (m2 == 0) mem[ip+3]+1 else rb+mem[ip+3]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        if (v1 == 0) ip <- v2 else ip <- ip + 3
      } else if (op == 7) {
        v1 <- if (m1 == 1) mem[ip+2] else { idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        v2 <- if (m2 == 1) mem[ip+3] else { idx <- if (m2 == 0) mem[ip+3]+1 else rb+mem[ip+3]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        p3 <- if (m3 == 0) mem[ip+4]+1 else rb+mem[ip+4]+1
        mem[p3] <- if (v1 < v2) 1 else 0; ip <- ip + 4
      } else if (op == 8) {
        v1 <- if (m1 == 1) mem[ip+2] else { idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        v2 <- if (m2 == 1) mem[ip+3] else { idx <- if (m2 == 0) mem[ip+3]+1 else rb+mem[ip+3]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        p3 <- if (m3 == 0) mem[ip+4]+1 else rb+mem[ip+4]+1
        mem[p3] <- if (v1 == v2) 1 else 0; ip <- ip + 4
      } else if (op == 9) {
        v1 <- if (m1 == 1) mem[ip+2] else { idx <- if (m1 == 0) mem[ip+2]+1 else rb+mem[ip+2]+1; if (idx > length(mem) || is.na(mem[idx])) 0 else mem[idx] }
        rb <- rb + v1; ip <- ip + 2
      } else if (op == 99) {
        c$mem <- mem; c$ip <- ip; c$rb <- rb; c$inputs <- inputs; break
      }
    }
  }
}