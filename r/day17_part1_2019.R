
decode <- function(n) {
  op <- n %% 100
  modes <- c(floor(n / 100) %% 10, floor(n / 1000) %% 10, floor(n / 10000) %% 10)
  return(list(op, modes))
}

Machine <- function(program, in_stream) {
  data <- setNames(as.list(program), seq_along(program) - 1)
  ip <- 0
  relbase <- 0
  out_stream <- list()

  get <- function(i, mo) {
    idx <- i -1
    if (mo == 0) {
        return(data[[as.character(data[[as.character(idx)]]  )]]  %||% 0)
    } else if (mo == 1) {
      return(data[[as.character(idx)]] %||% 0)
    } else if (mo == 2) {
      return(data[[as.character(relbase + data[[as.character(idx)]])]] %||% 0 )
    }
  }

  set <- function(i, mo, val) {
        idx <- i-1
    if (mo == 0) {
      data[[as.character(data[[as.character(idx)]])]] <<- val
    } else if (mo == 2) {
      data[[as.character(relbase + data[[as.character(idx)]])]] <<- val
    }
  }

  step <- function() {
    op_modes <- decode(data[[as.character(ip)]] %||% 0)
    op <- op_modes[[1]]
    modes <- op_modes[[2]]

    if (op == 1) {
      set(ip + 4, modes[3], get(ip + 2, modes[1]) + get(ip + 3, modes[2]))
      ip <<- ip + 4
    } else if (op == 2) {
      set(ip + 4, modes[3], get(ip + 2, modes[1]) * get(ip + 3, modes[2]))
      ip <<- ip + 4
    } else if (op == 3) {
      set(ip + 2, modes[1], in_stream[[1]])
      in_stream <<- in_stream[-1]
      ip <<- ip + 2
    } else if (op == 4) {
      out_stream <<- c(out_stream, get(ip + 2, modes[1]))
      ip <<- ip + 2
    } else if (op == 5) {
      if (get(ip + 2, modes[1]) != 0) {
        ip <<- get(ip + 3, modes[2])
      } else {
        ip <<- ip + 3
      }
    } else if (op == 6) {
      if (get(ip + 2, modes[1]) == 0) {
        ip <<- get(ip + 3, modes[2])
      } else {
        ip <<- ip + 3
      }
    } else if (op == 7) {
      set(ip + 4, modes[3], as.integer(get(ip + 2, modes[1]) < get(ip + 3, modes[2])))
      ip <<- ip + 4
    } else if (op == 8) {
      set(ip + 4, modes[3], as.integer(get(ip + 2, modes[1]) == get(ip + 3, modes[2])))
      ip <<- ip + 4
    } else if (op == 9) {
      relbase <<- relbase + get(ip + 2, modes[1])
      ip <<- ip + 2
    } else if (op == 99) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  run <- function(){
    while(step()){}
    return(unlist(out_stream))
  }

  return(list(run = run))
}

parse <- function(program) {
    out <- Machine(program, list())$run()
    scaffolding <- list()
    x <- 0
    y <- 0

     for (o in out) {
        c <- intToUtf8(o)
        if (c == '\n') {
            y <- y + 1
            x <- 0
        } else {
            if (c %in% c('^', 'v', '<', '>','#')) {
                scaffolding[[paste(x, y, sep=",")]] <- '#'
            }
            x <- x+1
        }
     }
    return(scaffolding)
}

sum_align <- function(grid) {
  sum_ <- 0
  for (coords in names(grid)) {
    xy <- as.integer(strsplit(coords, ",")[[1]])
    x <- xy[1]
    y <- xy[2]
    neighbors <- list(
      c(x, y + 1),
      c(x, y - 1),
      c(x + 1, y),
      c(x - 1, y)
    )
    is_intersection <- TRUE
    for (n in neighbors) {
      if (is.null(grid[[paste(n, collapse = ",")]])) {
        is_intersection <- FALSE
        break
      }
    }    
    if (is_intersection) {
      sum_ <- sum_ + (x * y)
    }
  }
  return(sum_)
}

main <- function() {
  program <- as.integer(strsplit(readLines("input.txt"), ",")[[1]])
  scaffolding <- parse(program)
  cat(sum_align(scaffolding), "\n")
}

main()
