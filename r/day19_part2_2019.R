
VM <- function(filename) {
  code <- numeric(4096)
  ip <- 1
  input <- c()
  output <- c()
  relative_base <- 0
  
  data <- as.integer(strsplit(readLines(filename), ",")[[1]])
  code[1:length(data)] <- data
  
  run <- function() {
    while (TRUE) {
      cmd <- code[ip] %% 100
      mode <- function(pos) (code[ip] %/% 10^(pos+1)) %% 10
      
      param <- function(pos) {
          m <- mode(pos)
        addr <- if (m == 0) code[ip + pos] + 1 else if (m == 1) ip + pos else relative_base + code[ip + pos] + 1
        
        return (ifelse(addr > length(code) | addr < 0 , 0, code[addr]))
      }
      
      set_param <- function(pos, value) {
        m <- mode(pos)
        addr <- if (m == 0) code[ip + pos] + 1 else  relative_base + code[ip + pos] + 1
        
        if (addr > 0) {    
            if (addr > length(code))  code <<- c(code, rep(0, addr - length(code)))
            code[addr] <<- value
        }
      }
      
      if (cmd == 1) {
        set_param(3, param(1) + param(2))
        ip <- ip + 4
      } else if (cmd == 2) {
        set_param(3, param(1) * param(2))
        ip <- ip + 4
      } else if (cmd == 3) {
        set_param(1, input[1])
        input <<- input[-1]
        ip <- ip + 2
      } else if (cmd == 4) {
        output <<- c(output, param(1))
        ip <- ip + 2
      } else if (cmd == 5) {
        if (param(1) != 0) {
          ip <- param(2) + 1
        } else {
          ip <- ip + 3
        }
      } else if (cmd == 6) {
        if (param(1) == 0) {
          ip <- param(2) + 1
        } else {
          ip <- ip + 3
        }
      } else if (cmd == 7) {
        set_param(3, ifelse(param(1) < param(2), 1, 0))
        ip <- ip + 4
      } else if (cmd == 8) {
        set_param(3, ifelse(param(1) == param(2), 1, 0))
        ip <- ip + 4
      } else if (cmd == 9) {
        relative_base <- relative_base + param(1)
        ip <- ip + 2
      } else if (cmd == 99) {
        return()
      }
    }
  }
  
  list(run = run, input = function(v) input <<- c(input, v), output = function() output)
}

beam <- function(x, y) {
  vm <- VM("input.txt")
  vm$input(c(x, y))
  vm$run()
  tail(vm$output(), 1) == 1
}

main <- function() {
    y <- 20
    x <- 0

    while (TRUE) {
        if (!beam(x, y)) {
            x <- x + 1
            next
        }

        if (!beam(x + 99, y)) {
            y <- y + 1
            next
        }
        
        if (!beam(x, y + 99))
        {
            x <- x + 1
            next
        }

        cat(x * 10000 + y, "\n")
        return()
    }
}

main()
