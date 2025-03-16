
parse_mirror <- function(mirror_str) {
    rows <- integer(length(mirror_str))
    cols <- integer(nchar(mirror_str[1]))

    for (y in seq_along(mirror_str)) {
        for (x in 1:nchar(mirror_str[y])) {
            char <- substr(mirror_str[y], x, x)
            rows[y] <- bitwShiftL(rows[y], 1)
            cols[x] <- bitwShiftL(cols[x], 1)
            if (char == '#') {
                rows[y] <- rows[y] + 1
                cols[x] <- cols[x] + 1
            }
        }
    }
    list(Rows = rows, Cols = cols)
}

parse_input <- function(input_lines) {
    mirrors <- list()
    mirror_str <- character()

    for (line in input_lines) {
        if (line == "") {
            mirrors[[length(mirrors) + 1]] <- parse_mirror(mirror_str)
            mirror_str <- character()
        } else {
            mirror_str <- c(mirror_str, line)
        }
    }
    mirrors[[length(mirrors) + 1]] <- parse_mirror(mirror_str)
    mirrors
}

get_mirror_axis <- function(lines) {
    for (i in 2:length(lines)) {
        is_mirror <- TRUE
        for (j in 1:min(i - 1, length(lines) - i +1)) {
            if (lines[i - j] != lines[i + j -1]) {
                is_mirror <- FALSE
                break
            }
        }
        if (is_mirror) {
            return(i - 1)
        }
    }
    0
}

main <- function() {
    input <- readLines("input.txt")
    mirrors <- parse_input(input)

    res <- 0
    for (mirror in mirrors) {
        res <- res + get_mirror_axis(mirror$Cols)
        res <- res + get_mirror_axis(mirror$Rows) * 100
    }
    cat(res)
}

main()
