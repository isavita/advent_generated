
main <- function() {
    input_str <- readChar("input.txt", file.info("input.txt")$size)
    lines <- strsplit(trimws(input_str), "\n")[[1]]

    ground <- matrix("+", nrow = 1, ncol = 1)
    max_x <- 0
    min_x <- 0
    max_y <- 0
    min_y <- 20
    x_offset <- 500
    y_offset <- 0

    for (line in lines) {
        split <- strsplit(line, "[=, .]+")[[1]]
        if (split[1] == "x") {
            x <- as.integer(split[2]) - x_offset
            y1 <- as.integer(split[4]) - y_offset
            y2 <- as.integer(split[5]) - y_offset

            while (x >= max_x) {
                max_x <- max_x + 1
                ground <- cbind(ground, rep(".", nrow(ground)))
            }
            while (x <= min_x) {
                min_x <- min_x - 1
                ground <- cbind(rep(".", nrow(ground)), ground)
            }
            while (y2 > max_y) {
                max_y <- max_y + 1
                ground <- rbind(ground, rep(".", ncol(ground)))
            }
            if (y1 < min_y) {
                min_y <- y1
            }
            for (i in y1:y2) {
                ground[i + 1, x - min_x + 1] <- "#"
            }
        } else {
            y <- as.integer(split[2]) - y_offset
            x1 <- as.integer(split[4]) - x_offset
            x2 <- as.integer(split[5]) - x_offset

            while (y > max_y) {
                max_y <- max_y + 1
                ground <- rbind(ground, rep(".", ncol(ground)))
            }
            while (x2 >= max_x) {
                max_x <- max_x + 1
                ground <- cbind(ground, rep(".", nrow(ground)))
            }
            while (x1 <= min_x) {
                min_x <- min_x - 1
                ground <- cbind(rep(".", nrow(ground)), ground)
            }
            for (i in x1:x2) {
                ground[y + 1, i - min_x + 1] <- "#"
            }
            if (y < min_y) {
                min_y <- y
            }
        }
    }

    water_count <- 0
    round_limit <- 200000

    while (ground[2, -min_x + 1] != "|" && water_count < round_limit) {
        can_move <- TRUE
        x <- -min_x + 1
        y <- 2
        try_left <- 0
        while (can_move) {
          
            if (y + 1 > max_y + 1 || ground[y + 1, x] == "|") {
                ground[y, x] <- "|"
                can_move <- FALSE
            } else if (ground[y + 1, x] == ".") {
                y <- y + 1
                try_left <- 0
            } else if (ground[y + 1, x] == "#" || ground[y + 1, x] == "~") {
              
                left_ok <- (try_left == 1 && ground[y, x - 1] == "|")
                right_ok <- (try_left == 2 && ground[y, x + 1] == "|")
                sides_pipe <- (ground[y,x+1] == "|" && ground[y,x-1] != ".")
                sides_other <- (ground[y, x + 1] != "." && ground[y, x - 1] == "|")

                if (left_ok || right_ok || sides_pipe || sides_other) {

                    ground[y, x] <- "|"
                    can_move <- FALSE
                    i <- x + 1
                    while (i <= ncol(ground) && ground[y, i] == "~") {
                        ground[y, i] <- "|"
                        water_count <- water_count - 1
                        i <- i + 1
                    }
                    i <- x - 1
                    while (i >= 1 && ground[y, i] == "~") {
                        ground[y, i] <- "|"
                        water_count <- water_count - 1
                        i <- i - 1
                    }
                } else if ((try_left == 0 && ground[y, x - 1] == ".") ||
                           (try_left == 1 && ground[y, x - 1] == ".")) {
                    x <- x - 1
                    try_left <- 1
                } else if ((try_left == 0 && ground[y, x + 1] == ".") ||
                           (try_left == 2 && ground[y, x + 1] == ".")) {
                    x <- x + 1
                    try_left <- 2
                } else {
                    can_move <- FALSE
                    ground[y, x] <- "~"
                    water_count <- water_count + 1
                }
            }
        }
    }
      
    print(sum(ground == "~"))
}

main()
