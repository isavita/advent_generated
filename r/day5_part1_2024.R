
file_conn <- file("input.txt", "r")
lines <- readLines(file_conn)
close(file_conn)
lines <- lines[lines != ""]

rules <- list()
updates <- list()

i <- 1
while (i <= length(lines) && grepl("\\|", lines[i])) {
    parts <- strsplit(lines[i], "\\|")[[1]]
    rules[[length(rules) + 1]] <- c(as.integer(parts[1]), as.integer(parts[2]))
    i <- i + 1
}

while (i <= length(lines)) {
    parts <- strsplit(lines[i], ",")[[1]]
    updates[[length(updates) + 1]] <- as.integer(parts)
    i <- i + 1
}

is_correct <- function(update) {
    pos <- setNames(seq_along(update), update)
    for (rule in rules) {
        x <- rule[1]
        y <- rule[2]
        if (x %in% names(pos) && y %in% names(pos)) {
            if (pos[as.character(x)] > pos[as.character(y)]) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

s <- 0
for (u in updates) {
    if (is_correct(u)) {
        s <- s + u[length(u) %/% 2 + 1]
    }
}

cat(s)
