
options(scipen = 999)

get_perms <- function(x) {
  if (length(x) == 1) return(list(x))
  do.call(c, lapply(seq_along(x), function(i) {
    lapply(get_perms(x[-i]), function(p) c(x[i], p))
  }))
}

run_vm <- function(vm) {
  while (TRUE) {
    ins <- vm$code[vm$ip + 1]
    op <- ins %% 100
    if (op == 99) {
      vm$halted <- TRUE
      return(list(vm = vm, type = "halt"))
    }
    m <- c((ins %/% 100) %% 10, (ins %/% 1000) %% 10)
    
    if (op == 1) {
      v1 <- if (m[1] == 0) vm$code[vm$code[vm$ip + 2] + 1] else vm$code[vm$ip + 2]
      v2 <- if (m[2] == 0) vm$code[vm$code[vm$ip + 3] + 1] else vm$code[vm$ip + 3]
      vm$code[vm$code[vm$ip + 4] + 1] <- v1 + v2
      vm$ip <- vm$ip + 4
    } else if (op == 2) {
      v1 <- if (m[1] == 0) vm$code[vm$code[vm$ip + 2] + 1] else vm$code[vm$ip + 2]
      v2 <- if (m[2] == 0) vm$code[vm$code[vm$ip + 3] + 1] else vm$code[vm$ip + 3]
      vm$code[vm$code[vm$ip + 4] + 1] <- v1 * v2
      vm$ip <- vm$ip + 4
    } else if (op == 3) {
      if (length(vm$inputs) == 0) return(list(vm = vm, type = "wait"))
      vm$code[vm$code[vm$ip + 2] + 1] <- vm$inputs[1]
      vm$inputs <- vm$inputs[-1]
      vm$ip <- vm$ip + 2
    } else if (op == 4) {
      v1 <- if (m[1] == 0) vm$code[vm$code[vm$ip + 2] + 1] else vm$code[vm$ip + 2]
      vm$ip <- vm$ip + 2
      return(list(vm = vm, type = "output", val = v1))
    } else if (op == 5) {
      v1 <- if (m[1] == 0) vm$code[vm$code[vm$ip + 2] + 1] else vm$code[vm$ip + 2]
      v2 <- if (m[2] == 0) vm$code[vm$code[vm$ip + 3] + 1] else vm$code[vm$ip + 3]
      if (v1 != 0) vm$ip <- v2 else vm$ip <- vm$ip + 3
    } else if (op == 6) {
      v1 <- if (m[1] == 0) vm$code[vm$code[vm$ip + 2] + 1] else vm$code[vm$ip + 2]
      v2 <- if (m[2] == 0) vm$code[vm$code[vm$ip + 3] + 1] else vm$code[vm$ip + 3]
      if (v1 == 0) vm$ip <- v2 else vm$ip <- vm$ip + 3
    } else if (op == 7) {
      v1 <- if (m[1] == 0) vm$code[vm$code[vm$ip + 2] + 1] else vm$code[vm$ip + 2]
      v2 <- if (m[2] == 0) vm$code[vm$code[vm$ip + 3] + 1] else vm$code[vm$ip + 3]
      vm$code[vm$code[vm$ip + 4] + 1] <- as.numeric(v1 < v2)
      vm$ip <- vm$ip + 4
    } else if (op == 8) {
      v1 <- if (m[1] == 0) vm$code[vm$code[vm$ip + 2] + 1] else vm$code[vm$ip + 2]
      v2 <- if (m[2] == 0) vm$code[vm$code[vm$ip + 3] + 1] else vm$code[vm$ip + 3]
      vm$code[vm$code[vm$ip + 4] + 1] <- as.numeric(v1 == v2)
      vm$ip <- vm$ip + 4
    }
  }
}

code <- scan("input.txt", sep = ",", quiet = TRUE)
padded_code <- rep(0, 4096)
padded_code[1:length(code)] <- code

all_phases <- get_perms(5:9)
max_val <- 0

for (p in all_phases) {
  vms <- lapply(1:5, function(i) {
    list(code = padded_code, ip = 0, halted = FALSE, inputs = p[i])
  })
  vms[[1]]$inputs <- c(vms[[1]]$inputs, 0)
  
  amp_idx <- 1
  last_e <- 0
  while (!vms[[5]]$halted) {
    res <- run_vm(vms[[amp_idx]])
    vms[[amp_idx]] <- res$vm
    if (res$type == "output") {
      nxt <- (amp_idx %% 5) + 1
      vms[[nxt]]$inputs <- c(vms[[nxt]]$inputs, res$val)
      if (amp_idx == 5) last_e <- res$val
    }
    amp_idx <- (amp_idx %% 5) + 1
  }
  if (last_e > max_val) max_val <- last_e
}

cat(max_val, "\n")
