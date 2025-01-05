
input <- readLines("input.txt")
tracks <- strsplit(input, "")
carts <- list()
for (y in seq_along(tracks)) {
  for (x in seq_along(tracks[[y]])) {
    char <- tracks[[y]][x]
    if (char %in% c(">", "<", "^", "v")) {
      carts <- append(carts, list(list(x = x, y = y, direction = char, turns = 0)))
      if (char %in% c(">", "<")) {
        tracks[[y]][x] <- "-"
      } else {
        tracks[[y]][x] <- "|"
      }
    }
  }
}

while (length(carts) > 1) {
  order <- order(sapply(carts, function(c) c$y), sapply(carts, function(c) c$x))
  carts <- carts[order]
  to_remove <- integer()
  for (i in seq_along(carts)) {
    if (i %in% to_remove) {
      next
    }
    cart <- carts[[i]]
    
    move_cart <- function(cart, tracks) {
      if (cart$direction == ">") {
        cart$x <- cart$x + 1
      } else if (cart$direction == "<") {
        cart$x <- cart$x - 1
      } else if (cart$direction == "^") {
        cart$y <- cart$y - 1
      } else if (cart$direction == "v") {
        cart$y <- cart$y + 1
      }
      
      track_char <- tracks[[cart$y]][cart$x]
      if (track_char == "+") {
        turn_cart <- function(cart) {
          if (cart$turns %% 3 == 0) {
            if (cart$direction == ">") {
              cart$direction <- "^"
            } else if (cart$direction == "<") {
              cart$direction <- "v"
            } else if (cart$direction == "^") {
              cart$direction <- "<"
            } else {
              cart$direction <- ">"
            }
          } else if (cart$turns %% 3 == 2) {
            if (cart$direction == ">") {
              cart$direction <- "v"
            } else if (cart$direction == "<") {
              cart$direction <- "^"
            } else if (cart$direction == "^") {
              cart$direction <- ">"
            } else {
              cart$direction <- "<"
            }
          }
          cart$turns <- cart$turns + 1
          return(cart)
        }
        cart <- turn_cart(cart)
      } else if (track_char %in% c("/", "\\")) {
        change_direction <- function(cart, track_char) {
          if (track_char == "/") {
            if (cart$direction == ">") {
              cart$direction <- "^"
            } else if (cart$direction == "<") {
              cart$direction <- "v"
            } else if (cart$direction == "^") {
              cart$direction <- ">"
            } else {
              cart$direction <- "<"
            }
          } else if (track_char == "\\") {
            if (cart$direction == ">") {
              cart$direction <- "v"
            } else if (cart$direction == "<") {
              cart$direction <- "^"
            } else if (cart$direction == "^") {
              cart$direction <- "<"
            } else {
              cart$direction <- ">"
            }
          }
          return(cart)
        }
        cart <- change_direction(cart, track_char)
      }
      return(cart)
    }
    
    carts[[i]] <- move_cart(cart, tracks)
    
    check_crash <- function(cart, carts, current_index) {
      for (j in seq_along(carts)) {
        if (j != current_index && carts[[j]]$x == cart$x && carts[[j]]$y == cart$y) {
          return(j)
        }
      }
      return(-1)
    }
    
    crash_index <- check_crash(carts[[i]], carts, i)
    if (crash_index != -1) {
      to_remove <- c(to_remove, i, crash_index)
    }
  }
  if(length(to_remove) > 0){
    carts <- carts[-unique(to_remove)]
  }
}

cat(carts[[1]]$x - 1, ",", carts[[1]]$y - 1, sep = "")
