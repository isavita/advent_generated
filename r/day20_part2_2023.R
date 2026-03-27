
options(scipen = 999)

gcd <- function(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}

lcm <- function(a, b) {
  if (a == 0 || b == 0) return(0)
  return(abs(a * b) / gcd(a, b))
}

lines <- readLines("input.txt")
modules <- list()

for (line in lines) {
  parts <- strsplit(line, " -> ")[[1]]
  name_part <- parts[1]
  outputs <- strsplit(parts[2], ", ")[[1]]
  
  if (name_part == "broadcaster") {
    name <- "broadcaster"
    type <- "broadcaster"
  } else if (startsWith(name_part, "%")) {
    name <- substring(name_part, 2)
    type <- "flipflop"
  } else if (startsWith(name_part, "&")) {
    name <- substring(name_part, 2)
    type <- "conjunction"
  }
  
  modules[[name]] <- list(type = type, outputs = outputs, state = FALSE, memory = list())
}

# Initialize conjunction memories
for (name in names(modules)) {
  for (out in modules[[name]]$outputs) {
    if (!is.null(modules[[out]]) && modules[[out]]$type == "conjunction") {
      modules[[out]]$memory[[name]] <- FALSE
    }
  }
}

# Part 2 Specifics
rx_feeder <- ""
for (name in names(modules)) {
  if ("rx" %in% modules[[name]]$outputs) {
    rx_feeder <- name
    break
  }
}

cycle_inputs <- names(modules[[rx_feeder]]$memory)
loop_lengths <- list()
press_count <- 0

while (length(loop_lengths) < length(cycle_inputs)) {
  press_count <- press_count + 1
  queue <- list(list(from = "button", to = "broadcaster", pulse = FALSE))
  
  head <- 1
  while (head <= length(queue)) {
    curr <- queue[[head]]
    head <- head + 1
    
    from <- curr$from
    to <- curr$to
    pulse <- curr$pulse
    
    if (to == rx_feeder && pulse) {
      if (is.null(loop_lengths[[from]])) {
        loop_lengths[[from]] <- press_count
      }
    }
    
    if (is.null(modules[[to]])) next
    
    m <- modules[[to]]
    send_pulse <- NULL
    
    if (m$type == "broadcaster") {
      send_pulse <- pulse
    } else if (m$type == "flipflop") {
      if (!pulse) {
        modules[[to]]$state <- !modules[[to]]$state
        send_pulse <- modules[[to]]$state
      }
    } else if (m$type == "conjunction") {
      modules[[to]]$memory[[from]] <- pulse
      if (all(unlist(modules[[to]]$memory))) {
        send_pulse <- FALSE
      } else {
        send_pulse <- TRUE
      }
    }
    
    if (!is.null(send_pulse)) {
      for (out in modules[[to]]$outputs) {
        queue[[length(queue) + 1]] <- list(from = to, to = out, pulse = send_pulse)
      }
    }
  }
  
  if (press_count > 10000) break # Safety
}

ans <- 1
for (val in unlist(loop_lengths)) {
  ans <- lcm(ans, val)
}

cat(ans, "\n")
