
read_input <- function(filename) {
  readChar(filename, file.info(filename)$size)
}

max_pressure <- function(valves, curr, minute, pressure, open) {
  max_val <- pressure
  for (next_valve in open) {
    new_open <- open[open != next_valve]
    time_left <- minute - valves[[curr]][["tunnels"]][[next_valve]] - 1
    if (time_left > 0) {
      max_val <- max(max_val, max_pressure(valves, next_valve, time_left, time_left * valves[[next_valve]][["flow"]] + pressure, new_open))
    }
  }
  return(max_val)
}

main <- function() {
  valves <- list()
  input_data <- read_input("input.txt")
  
  for (line in strsplit(input_data, "\n")[[1]]) {
    sp <- strsplit(line, "; ")[[1]]
    v <- list()
    v[["id"]] <- strsplit(sp[1], " ")[[1]][2]
    v[["flow"]] <- as.integer(strsplit(sp[1], "=")[[1]][2])
    sp[2] <- substr(sp[2], nchar("tunnel leads to valve") + 1, nchar(sp[2]))
    if (startsWith(sp[2], "s")) {
      sp[2] <- substr(sp[2], 3, nchar(sp[2]))
    } else {
      sp[2] <- substr(sp[2], 2, nchar(sp[2]))
    }
    v[["tunnels"]] <- list()
    v[["tunnels"]][[v[["id"]]]] <- 0
    for (t in strsplit(sp[2], ", ")[[1]]) {
      v[["tunnels"]][[t]] <- 1
    }
    valves[[v[["id"]]]] <- v
  }
  
  for (k in names(valves)) {
    for (i in names(valves)) {
      for (j in names(valves)) {
        dik <- valves[[i]][["tunnels"]][[k]]
        dkj <- valves[[k]][["tunnels"]][[j]]
        if (!is.null(dik) && !is.null(dkj)) {
          dij <- valves[[i]][["tunnels"]][[j]]
          if (is.null(dij) || dij > dik + dkj) {
            valves[[i]][["tunnels"]][[j]] <- dik + dkj
          }
        }
      }
    }
  }
  
  open_valves <- names(valves)[sapply(valves, function(v) v[["flow"]] > 0)]
  
  cat(max_pressure(valves, "AA", 30, 0, open_valves))
}

main()
