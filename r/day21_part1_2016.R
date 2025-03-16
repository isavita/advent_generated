
swap_position <- function(password, x, y) {
    x <- x + 1
    y <- y + 1
    s <- strsplit(password, "")[[1]]
    tmp <- s[x]
    s[x] <- s[y]
    s[y] <- tmp
    paste(s, collapse = "")
}

swap_letter <- function(password, x, y) {
  s <- strsplit(password, "")[[1]]
  idx_x <- which(s == x)
  idx_y <- which(s == y)
  s[idx_x] <- y
  s[idx_y] <- x
  paste(s, collapse = "")
}

rotate_left <- function(password, steps) {
    len <- nchar(password)
    steps <- steps %% len
    paste0(substr(password, steps + 1, len), substr(password, 1, steps))
}

rotate_right <- function(password, steps) {
    len <- nchar(password)
    steps <- steps %% len
    paste0(substr(password, len - steps + 1, len), substr(password, 1, len - steps))
}

rotate_based_on_position <- function(password, x) {
    s <- strsplit(password, "")[[1]]
    index <- which(s == x) -1
    steps <- 1 + index + ifelse(index >= 4, 1, 0)
    rotate_right(password, steps)
}

reverse_positions <- function(password, x, y) {
    x <- x + 1
    y <- y + 1
    substr(password, x, y) <- paste(rev(strsplit(substr(password, x, y), "")[[1]]), collapse = "")
    password
}

move_position <- function(password, x, y) {
    x <- x + 1
    y <- y + 1
    s <- strsplit(password, "")[[1]]
    char <- s[x]
    s <- s[-x]
    if (y > length(s) + 1)
        s <- c(s,char)
    else
        s <- append(s, char, after = y -1)
    
    paste(s, collapse = "")
}

apply_operation <- function(op, password) {
    fields <- strsplit(op, " ")[[1]]
    if (fields[1] == "swap") {
        if (fields[2] == "position") {
            password <- swap_position(password, as.integer(fields[3]), as.integer(fields[6]))
        } else if (fields[2] == "letter") {
            password <- swap_letter(password, fields[3], fields[6])
        }
    } else if (fields[1] == "rotate") {
        if (fields[2] == "left") {
            password <- rotate_left(password, as.integer(fields[3]))
        } else if (fields[2] == "right") {
            password <- rotate_right(password, as.integer(fields[3]))
        } else if (fields[2] == "based") {
            password <- rotate_based_on_position(password, fields[7])
        }
    } else if (fields[1] == "reverse") {
        password <- reverse_positions(password, as.integer(fields[3]), as.integer(fields[5]))
    } else if (fields[1] == "move") {
        password <- move_position(password, as.integer(fields[3]), as.integer(fields[6]))
    }
    password
}

main <- function() {
    operations <- readLines("input.txt")
    password <- "abcdefgh"
    for (op in operations) {
        password <- apply_operation(op, password)
    }
    cat(password)
}

main()
