
match_hands <- function(hands) {
  matches <- list(list(), list(), list(), list(), list(), list(), list())
  for (hand in hands) {
    counts <- table(strsplit(hand$cards, "")[[1]])
    value <- prod(counts)
    if (value == 1)      matches[[7]] <- append(matches[[7]], list(hand))
    else if (value == 2) matches[[6]] <- append(matches[[6]], list(hand))
    else if (value == 3) matches[[4]] <- append(matches[[4]], list(hand))
    else if (value == 4) {
      if (length(counts) == 2) matches[[2]] <- append(matches[[2]], list(hand))
      else                     matches[[5]] <- append(matches[[5]], list(hand))
    }
    else if (value == 5) matches[[1]] <- append(matches[[1]], list(hand))
    else if (value == 6) matches[[3]] <- append(matches[[3]], list(hand))
  }
  matches
}

convert_order <- function(matches) {
    
  converted <- lapply(matches, function(category) {
    ranks <- sapply(category, function(hand) {
      cards <- gsub("A", "E", hand$cards)
      cards <- gsub("T", "A", cards)
      cards <- gsub("J", "B", cards)
      cards <- gsub("Q", "C", cards)
      cards <- gsub("K", "D", cards)
      strtoi(cards, base = 16)
    })
    
    category[order(ranks,decreasing = TRUE)]
  })

    do.call(c,converted)

}

main <- function() {
  lines <- readLines("input.txt")
  hands <- lapply(lines, function(line) {
    if (nchar(line) == 0) return(NULL)
    parts <- strsplit(line, " ")[[1]]
    list(cards = parts[1], bid = as.integer(parts[2]))
  })
  hands<-hands[!sapply(hands, is.null)]

  matches <- match_hands(hands)
  converted_matches <- convert_order(matches)
  
    total <- sum(sapply(seq_along(converted_matches), function(i) {
    converted_matches[[i]]$bid * (length(converted_matches) - i + 1)
  }))
  
  cat(total)
}

main()
