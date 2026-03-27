
lines <- readLines("input.txt", warn = FALSE)
split_pt <- which(lines == "")[1]

wires <- new.env()
for (line in lines[1:(split_pt - 1)]) {
  s <- strsplit(line, ": ")[[1]]
  wires[[s[1]]] <- as.numeric(s[2])
}

gate_data <- lines[(split_pt + 1):length(lines)]
gate_data <- gate_data[gate_data != ""]
gates_list <- strsplit(gate_data, " ")
gates <- do.call(rbind, lapply(gates_list, function(x) x[c(1, 2, 3, 5)]))

pending <- rep(TRUE, nrow(gates))
changed <- TRUE

while (changed && any(pending)) {
  changed <- FALSE
  for (i in which(pending)) {
    in1 <- gates[i, 1]
    op  <- gates[i, 2]
    in2 <- gates[i, 3]
    out <- gates[i, 4]
    
    if (exists(in1, envir = wires) && exists(in2, envir = wires)) {
      v1 <- wires[[in1]]
      v2 <- wires[[in2]]
      
      res <- if (op == "AND") {
        bitwAnd(v1, v2)
      } else if (op == "OR") {
        bitwOr(v1, v2)
      } else if (op == "XOR") {
        bitwXor(v1, v2)
      }
      
      wires[[out]] <- res
      pending[i] <- FALSE
      changed <- TRUE
    }
  }
}

z_keys <- sort(ls(wires, pattern = "^z"))
z_vals <- sapply(z_keys, function(k) wires[[k]])

ans <- 0
p2 <- 1
for (v in z_vals) {
  if (v == 1) ans <- ans + p2
  p2 <- p2 * 2
}

cat(sprintf("%.0f\n", ans))
