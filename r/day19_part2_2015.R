solve <- function(input) {
  graph <- list()
  startingMaterial <- character(0)
  
  blocks <- strsplit(input, "\n\n")[[1]]
  startingMaterial <- unlist(strsplit(blocks[2], ""))
  
  for (l in strsplit(blocks[1], "\n")[[1]]) {
    parts <- strsplit(l, " => ")[[1]]
    if (!(parts[1] %in% names(graph))) {
      graph[[parts[1]]] <- character(0)
    }
    graph[[parts[1]]] <- c(graph[[parts[1]]], parts[2])
  }
  
  productToReactant <- list()
  for (react in names(graph)) {
    for (p in graph[[react]]) {
      if (is.null(productToReactant[[p]])) {
        productToReactant[[p]] <- react
      } else {
        stop("dup found")
      }
    }
  }
  
  allProducts <- names(productToReactant)
  
  start <- paste(startingMaterial, collapse = "")
  mol <- start
  steps <- 0
  
  while (mol != "e") {
    changeMade <- FALSE
    for (prod in sample(allProducts)) {
      count <- gregexpr(prod, mol)[[1]]
      if (any(count > 0)) {
        changeMade <- TRUE
        steps <- steps + sum(count > 0)
        mol <- gsub(prod, productToReactant[[prod]], mol)
        break
      }
    }
    
    if (!changeMade) {
      mol <- start
      steps <- 0
    }
  }
  
  return(steps)
}

input <- readLines("input.txt")
cat(solve(paste(input, collapse = "\n")))