
vm <- function(code, input_queue, output_queue) {
  ip <- 1
  get_param <- function(address, immediate) {
    if (immediate) code[address] else code[code[address] + 1]
  }
  is_immediate <- function(cmd, param_num) {
    (cmd %/% 10^(param_num + 1)) %% 10 == 1
  }
  
  while (TRUE) {
    cmd <- code[ip]
    opcode <- cmd %% 100
    
    if (opcode == 1) {
      param1 <- get_param(ip + 1, is_immediate(cmd, 1))
      param2 <- get_param(ip + 2, is_immediate(cmd, 2))
      address <- get_param(ip + 3, TRUE)
      code[address + 1] <- param1 + param2
      ip <- ip + 4
    } else if (opcode == 2) {
      param1 <- get_param(ip + 1, is_immediate(cmd, 1))
      param2 <- get_param(ip + 2, is_immediate(cmd, 2))
      address <- get_param(ip + 3, TRUE)
      code[address + 1] <- param1 * param2
      ip <- ip + 4
    } else if (opcode == 3) {
      address <- get_param(ip + 1, TRUE)
      code[address + 1] <- input_queue[1]
      input_queue <- input_queue[-1]
      ip <- ip + 2
    } else if (opcode == 4) {
      param1 <- get_param(ip + 1, is_immediate(cmd, 1))
      output_queue <- c(output_queue, param1)
      ip <- ip + 2
    } else if (opcode == 5) {
      param1 <- get_param(ip + 1, is_immediate(cmd, 1))
      param2 <- get_param(ip + 2, is_immediate(cmd, 2))
      if (param1 != 0) {
        ip <- param2 + 1
      } else {
        ip <- ip + 3
      }
    } else if (opcode == 6) {
      param1 <- get_param(ip + 1, is_immediate(cmd, 1))
      param2 <- get_param(ip + 2, is_immediate(cmd, 2))
      if (param1 == 0) {
        ip <- param2 + 1
      } else {
        ip <- ip + 3
      }
    } else if (opcode == 7) {
      param1 <- get_param(ip + 1, is_immediate(cmd, 1))
      param2 <- get_param(ip + 2, is_immediate(cmd, 2))
      address <- get_param(ip + 3, TRUE)
      code[address + 1] <- ifelse(param1 < param2, 1, 0)
      ip <- ip + 4
    } else if (opcode == 8) {
      param1 <- get_param(ip + 1, is_immediate(cmd, 1))
      param2 <- get_param(ip + 2, is_immediate(cmd, 2))
      address <- get_param(ip + 3, TRUE)
      code[address + 1] <- ifelse(param1 == param2, 1, 0)
      ip <- ip + 4
    } else if (opcode == 99) {
      return(output_queue)
    }
  }
}

run_amplifiers <- function(phase, code) {
  input <- 0
  for (i in 1:5) {
    input <- vm(code, c(phase[i], input), c())
  }
  return(input)
}

main <- function() {
  code <- scan("input.txt", sep = ",", what = integer())
  
  perms <- function(v) {
    n <- length(v)
    if (n == 1) {
      return(matrix(v, 1, 1))
    }
    res <- matrix(0, nrow = factorial(n), ncol = n)
    idx <- 1
    for (i in 1:n) {
      sub_perms <- perms(v[-i])
      for (j in 1:nrow(sub_perms)) {
        res[idx, ] <- c(v[i], sub_perms[j, ])
        idx <- idx + 1
      }
    }
    return(res)
  }
  
  phase_permutations <- perms(0:4)
  max_output <- 0
  
  for (i in 1:nrow(phase_permutations)) {
    output <- run_amplifiers(phase_permutations[i, ], code)
    max_output <- max(max_output, output)
  }
  
  cat(max_output, "\n")
}

main()
