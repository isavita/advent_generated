
read_input <- function() {
  readLines("input.txt")
}

is_register <- function(x) {
  x %in% c("a", "b", "c", "d")
}

get_value <- function(x, registers) {
  if (is_register(x)) {
    registers[[x]]
  } else {
    as.integer(x)
  }
}

execute_program <- function(instructions, registers) {
  i <- 1
  while (i <= length(instructions)) {
    parts <- strsplit(instructions[i], " ")[[1]]
    cmd <- parts[1]

    if (i + 5 <= length(instructions)) {
      pattern <- instructions[i:(i + 5)]
      p_parts <- lapply(pattern, function(x) strsplit(x, " ")[[1]])

      if (length(p_parts) == 6 &&
          p_parts[[1]][1] == "cpy" &&
          p_parts[[2]][1] == "inc" &&
          p_parts[[3]][1] == "dec" &&
          p_parts[[4]][1] == "jnz" &&
          p_parts[[5]][1] == "dec" &&
          p_parts[[6]][1] == "jnz") {
        cpy_x <- p_parts[[1]][2]
        cpy_y <- p_parts[[1]][3]
        inc_a <- p_parts[[2]][2]
        dec_c <- p_parts[[3]][2]
        jnz_c <- p_parts[[4]][2]
        jnz_c_offset <- as.integer(p_parts[[4]][3])
        dec_d <- p_parts[[5]][2]
        jnz_d <- p_parts[[6]][2]
        jnz_d_offset <- as.integer(p_parts[[6]][3])
        
        if (inc_a == "a" && dec_c == cpy_y && jnz_c == cpy_y && jnz_c_offset == -2 &&
            dec_d == "d" && jnz_d == "d" && jnz_d_offset == -5) {
          registers[["a"]] <- registers[["a"]] + registers[[cpy_x]] * registers[["d"]]
          registers[[cpy_y]] <- 0
          registers[["d"]] <- 0
          i <- i + 6
          next
        }
      }
    }
    
    if (cmd == "tgl") {
      x <- get_value(parts[2], registers)
      target_idx <- i + x
      if (target_idx >= 1 && target_idx <= length(instructions)) {
        target_parts <- strsplit(instructions[target_idx], " ")[[1]]
        if (length(target_parts) == 2) {
          if (target_parts[1] == "inc") {
            target_parts[1] <- "dec"
          } else {
            target_parts[1] <- "inc"
          }
        } else if (length(target_parts) == 3) {
          if (target_parts[1] == "jnz") {
            target_parts[1] <- "cpy"
          } else {
            target_parts[1] <- "jnz"
          }
        }
        instructions[target_idx] <- paste(target_parts, collapse = " ")
      }
      i <- i + 1
      next
    }

    if (cmd == "cpy") {
      x <- parts[2]
      y <- parts[3]
      if (is_register(y)) {
        registers[[y]] <- get_value(x, registers)
      }
      i <- i + 1
    } else if (cmd == "inc") {
      x <- parts[2]
      if (is_register(x)) {
        registers[[x]] <- registers[[x]] + 1
      }
      i <- i + 1
    } else if (cmd == "dec") {
      x <- parts[2]
      if (is_register(x)) {
        registers[[x]] <- registers[[x]] - 1
      }
      i <- i + 1
    } else if (cmd == "jnz") {
      x <- parts[2]
      y <- parts[3]
      if (get_value(x, registers) != 0) {
        i <- i + get_value(y, registers)
      } else {
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }
  registers
}

main <- function() {
  instructions <- read_input()
  registers <- list(a = 12, b = 0, c = 0, d = 0)
  result_registers <- execute_program(instructions, registers)
  cat(result_registers[["a"]], "\n")
}

main()
