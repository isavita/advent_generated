
parse_input <- function(input) {
  parts <- strsplit(input, "\n\n")[[1]]
  if (length(parts) != 2) return(NULL)
  
  lines <- strsplit(parts[2], "\n")[[1]]
  lines <- lines[lines != ""]
  
  gates <- lapply(lines, function(line) {
    parts <- strsplit(line, " -> ")[[1]]
    if (length(parts) != 2) return(NULL)
    gate_parts <- strsplit(parts[1], " ")[[1]]
    if (length(gate_parts) != 3) return(NULL)
    list(gate = list(a = gate_parts[1], op = gate_parts[2], b = gate_parts[3]), output = parts[2])
  })
  gates <- gates[!sapply(gates, is.null)]
  return(gates)
}

create_lookups <- function(gates) {
  lookup <- list()
  reverse_lookup <- list()
  
  for (g in gates) {
    output <- g$output
    gate <- g$gate
    lookup[[output]] <- gate
    inputs <- sort(c(gate$a, gate$b))
    key <- paste(inputs[1], gate$op, inputs[2], sep = "_")
    reverse_lookup[[key]] <- output
  }
  return(list(lookup = lookup, reverse_lookup = reverse_lookup))
}

swap <- function(pairs, gates, a, b) {
  pairs <- rbind(pairs, c(a, b))
  for (i in seq_along(gates)) {
    if (gates[[i]]$output == a) {
      gates[[i]]$output <- b
    } else if (gates[[i]]$output == b) {
      gates[[i]]$output <- a
    }
  }
  return(list(pairs = pairs, gates = gates))
}

get_reverse_lookup_key <- function(a, op, b) {
  inputs <- sort(c(a, b))
  paste(inputs[1], op, inputs[2], sep = "_")
}

solution <- function(gates) {
  pairs <- matrix(character(0), ncol = 2)
  num_z <- sum(grepl("^z", sapply(gates, function(g) g$output)))
  
  while (nrow(pairs) < 4) {
    adder <- ""
    carry <- ""
    lookups <- create_lookups(gates)
    lookup <- lookups$lookup
    reverse_lookup <- lookups$reverse_lookup
    
    for (i in 0:(num_z - 1)) {
      xi <- sprintf("x%02d", i)
      yi <- sprintf("y%02d", i)
      zi <- sprintf("z%02d", i)
      
      if (i == 0) {
        adder <- reverse_lookup[[get_reverse_lookup_key(xi, "XOR", yi)]]
        carry <- reverse_lookup[[get_reverse_lookup_key(xi, "AND", yi)]]
      } else {
        bit <- reverse_lookup[[get_reverse_lookup_key(xi, "XOR", yi)]]
        if (!is.null(bit)) {
          adder <- reverse_lookup[[get_reverse_lookup_key(bit, "XOR", carry)]]
          if (!is.null(adder)) {
            c1 <- reverse_lookup[[get_reverse_lookup_key(xi, "AND", yi)]]
            c2 <- reverse_lookup[[get_reverse_lookup_key(bit, "AND", carry)]]
            carry <- reverse_lookup[[get_reverse_lookup_key(c1, "OR", c2)]]
          }
        }
      }
      
      if (is.null(adder) || adder == "") {
        gate <- lookup[[zi]]
        bit_key <- get_reverse_lookup_key(xi, "XOR", yi)
        bit <- reverse_lookup[[bit_key]]
        if (!is.null(reverse_lookup[[get_reverse_lookup_key(gate$a, "XOR", carry)]])) {
          swap_result <- swap(pairs, gates, bit, gate$a)
          pairs <- swap_result$pairs
          gates <- swap_result$gates
          break
        } else if (!is.null(reverse_lookup[[get_reverse_lookup_key(gate$b, "XOR", carry)]])) {
          swap_result <- swap(pairs, gates, bit, gate$b)
          pairs <- swap_result$pairs
          gates <- swap_result$gates
          break
        }
      } else if (adder != zi) {
        swap_result <- swap(pairs, gates, adder, zi)
        pairs <- swap_result$pairs
        gates <- swap_result$gates
        break
      }
    }
  }
  
  result <- sort(as.vector(pairs))
  paste(result, collapse = ",")
}

main <- function() {
  input <- readChar("input.txt", file.info("input.txt")$size)
  gates <- parse_input(input)
  if (is.null(gates)) {
    cat("Error parsing input\n")
    return()
  }
  cat(solution(gates), "\n")
}

main()
