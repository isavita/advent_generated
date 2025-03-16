
scrambler <- function(pw) {
  structure(list(pw = strsplit(pw, "")[[1]]), class = "scrambler")
}

print.scrambler <- function(x, ...) {
  cat(paste(x$pw, collapse = ""), "\n")
}

swap_positions <- function(obj, x, y) {
  x <- x + 1
  y <- y + 1
  tmp <- obj$pw[x]
  obj$pw[x] <- obj$pw[y]
  obj$pw[y] <- tmp
  obj
}

swap_letters <- function(obj, x, y) {
  x_idx <- which(obj$pw == x)
  y_idx <- which(obj$pw == y)
  swap_positions(obj, x_idx - 1, y_idx - 1)
}

rotate <- function(obj, steps) {
  length <- length(obj$pw)
  steps <- steps %% length
  if (steps < 0) {
    steps <- steps + length
  }
  if (steps > 0) {
    obj$pw <- c(obj$pw[(length - steps + 1):length], obj$pw[1:(length - steps)])
  }
  obj
}

rotate_letter <- function(obj, x) {
    idx <- which(obj$pw == x) -1
    idx <- idx + ifelse(idx >= 4, 2, 1)
    rotate(obj, idx)
}

derotate_letter <- function(obj, x) {
  index <- which(obj$pw == x) - 1
  rot <- if (index %% 2 == 1) {
    -(index + 1) %/% 2
  } else if (index != 0) {
    (6 - index) %/% 2
  } else {
    -1
  }
    rotate(obj, rot)
}

reverse_range <- function(obj, x, y) {
  x <- x + 1
  y <- y + 1
  obj$pw[x:y] <- rev(obj$pw[x:y])
  obj
}

move <- function(obj, x, y) {
  x <- x + 1
  y <- y + 1
  ch <- obj$pw[x]
  obj$pw <- obj$pw[-x]
  if (y > length(obj$pw)) {
        obj$pw <- c(obj$pw, ch)
    } else {
        obj$pw <- append(obj$pw, ch, after = y -1)
    }

  obj
}

scramble_instr <- function(obj, instructions, direction) {
  if (direction < 0) {
    instructions <- rev(instructions)
  }

  for (instruction in instructions) {
    parts <- strsplit(instruction, " ")[[1]]
    if (startsWith(instruction, "swap")) {
      x <- parts[3]
      y <- parts[length(parts)]
      if (parts[2] == "position") {
        obj <- swap_positions(obj, as.integer(x), as.integer(y))
      } else {
        obj <- swap_letters(obj, x, y)
      }
    } else if (startsWith(instruction, "rotate")) {
      if (parts[2] == "based") {
        if (direction > 0) {
          obj <- rotate_letter(obj, parts[length(parts)])
        }
        else{
          obj <- derotate_letter(obj, parts[length(parts)])
        }
      } else {
        x <- as.integer(parts[3])
        if (parts[2] == "left") {
          x <- -x
        }
        if (direction < 0) {
          x <- -x
        }
        obj <- rotate(obj, x)
      }
    } else if (startsWith(instruction, "reverse")) {
      x <- parts[3]
      y <- parts[length(parts)]
      obj <- reverse_range(obj, as.integer(x), as.integer(y))
    } else if (startsWith(instruction, "move")) {
      x <- parts[3]
      y <- parts[length(parts)]
      xi <- as.integer(x)
      yi <- as.integer(y)
      if (direction < 0) {
        tmp <- xi
        xi <- yi
        yi <- tmp
      }
      obj <- move(obj, xi, yi)
    }
  }
  obj
}


unscramble <- function(obj, instructions) {
    scramble_instr(obj, instructions, -1)
}

main <- function() {
  instructions <- readLines("input.txt")
  hashed <- "fbgdceah"
  scrambler_obj <- scrambler(hashed)
  result <- unscramble(scrambler_obj, instructions)
  print(result)
}

main()
