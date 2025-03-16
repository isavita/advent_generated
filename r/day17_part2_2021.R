
file <- file("input.txt", "r")
line <- readLines(file, n = 1)
close(file)

parts <- strsplit(line, ", ")[[1]]
x_range <- as.integer(strsplit(substring(parts[1], 16), "\\.\\.")[[1]])
y_range <- as.integer(strsplit(substring(parts[2], 3), "\\.\\.")[[1]])

x_min <- x_range[1]
x_max <- x_range[2]
y_min <- y_range[1]
y_max <- y_range[2]

velocities <- list()

for (x_vel in 0:x_max) {
    for (y_vel in y_min:1000) {
        x_pos <- 0
        y_pos <- 0
        cur_x_vel <- x_vel
        cur_y_vel <- y_vel
        in_target_area <- FALSE

        while (x_pos <= x_max && y_pos >= y_min) {
            x_pos <- x_pos + cur_x_vel
            y_pos <- y_pos + cur_y_vel

            if (x_pos >= x_min && x_pos <= x_max && y_pos >= y_min && y_pos <= y_max) {
                in_target_area <- TRUE
                break
            }
            
            if (cur_x_vel > 0) {
              cur_x_vel <- cur_x_vel -1
            }

            cur_y_vel <- cur_y_vel - 1
        }

        if (in_target_area) {
          velocities[[paste0(x_vel, ",", y_vel)]] <- TRUE
        }
    }
}

cat(length(velocities))
