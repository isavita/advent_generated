someAssemblyRequired <- function(input) {
  wireToRule <- setNames(sapply(strsplit(input, "\n"), function(inst) strsplit(inst, " -> ")[[1]][1]), 
                                sapply(strsplit(input, "\n"), function(inst) strsplit(inst, " -> ")[[1]][2]))
  
  memoDFS <- function(entry, memo) {
    if (entry %in% names(memo)) return(memo[[entry]])
    if (grepl("^[0-9]+$", entry)) return(as.integer(entry))
    
    sourceRule <- wireToRule[[entry]]
    parts <- strsplit(sourceRule, " ")[[1]]
    
    result <- switch(length(parts),
                     `1` = memoDFS(parts[1], memo),
                     `2` = bitwNot(memoDFS(parts[2], memo)),
                     `3` = switch(parts[2],
                                   "AND" = bitwAnd(memoDFS(parts[1], memo), memoDFS(parts[3], memo)),
                                   "OR" = bitwOr(memoDFS(parts[1], memo), memoDFS(parts[3], memo)),
                                   "LSHIFT" = bitwShiftL(memoDFS(parts[1], memo), memoDFS(parts[3], memo)),
                                   "RSHIFT" = bitwShiftR(memoDFS(parts[1], memo), memoDFS(parts[3], memo))))
    
    memo[[entry]] <- result
    return(result)
  }
  
  memo <- new.env(parent = emptyenv())
  return(memoDFS("a", memo))
}

input <- trimws(readLines("input.txt"))
cat(someAssemblyRequired(input), "\n")