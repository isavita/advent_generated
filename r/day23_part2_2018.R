
parse_input <- function(file_path) {
    lines <- readLines(file_path)
    nanobots <- matrix(0, nrow = length(lines), ncol = 4)
    for (i in seq_along(lines)) {
        matches <- regmatches(lines[i], regexec("pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)", lines[i]))[[1]]
        nanobots[i, ] <- as.integer(matches[2:5])
    }
    colnames(nanobots) <- c("x", "y", "z", "r")
    return(nanobots)
}

manhattan_distance <- function(a, b) {
    return(sum(abs(a - b)))
}

part_one <- function(nanobots) {
    strongest <- nanobots[which.max(nanobots[, "r"]), ]
    distances <- apply(nanobots[, c("x", "y", "z")], 1, manhattan_distance, b = strongest[c("x", "y", "z")])
    return(sum(distances <= strongest["r"]))
}

min_distance_to_origin <- function(x, y, z, size) {
    dx <- ifelse(x > 0, x, ifelse(x + size - 1 < 0, -(x + size - 1), 0))
    dy <- ifelse(y > 0, y, ifelse(y + size - 1 < 0, -(y + size - 1), 0))
    dz <- ifelse(z > 0, z, ifelse(z + size - 1 < 0, -(z + size - 1), 0))
    return(dx + dy + dz)
}

part_two <- function(nanobots) {
  
    min_x <- min(nanobots[, "x"])
    max_x <- max(nanobots[, "x"])
    min_y <- min(nanobots[, "y"])
    max_y <- max(nanobots[, "y"])
    min_z <- min(nanobots[, "z"])
    max_z <- max(nanobots[, "z"])

    size <- 1
    while (size < max(max_x - min_x, max_y - min_y, max_z - min_z)) {
        size <- size * 2
    }

    heap <- list(c(0, min_distance_to_origin(min_x, min_y, min_z, size), size, min_x, min_y, min_z))

    best_distance <- Inf
    best_count <- -1

    while (length(heap) > 0) {
        current <- heap[[1]]
        heap <- heap[-1]
        neg_count <- current[1]
        distance <- current[2]
        size <- current[3]
        x <- current[4]
        y <- current[5]
        z <- current[6]
        count <- -neg_count

        if (size == 1) {
            if (count > best_count || (count == best_count && distance < best_distance)) {
                best_count <- count
                best_distance <- distance
                break
            }
            next
        }

        half <- size %/% 2
        for (dx in c(0, half)) {
            for (dy in c(0, half)) {
                for (dz in c(0, half)) {
                    nx <- x + dx
                    ny <- y + dy
                    nz <- z + dz
                    new_size <- max(1, half)

                    count <- 0
                    for (i in 1:nrow(nanobots)) {
                        bot <- nanobots[i, ]
                        bx <- bot["x"]
                        by <- bot["y"]
                        bz <- bot["z"]
                        br <- bot["r"]

                        d <- 0
                        if (bx < nx) {
                            d <- d + nx - bx
                        } else if (bx > nx + new_size - 1) {
                            d <- d + bx - (nx + new_size - 1)
                        }
                        if (by < ny) {
                            d <- d + ny - by
                        } else if (by > ny + new_size - 1) {
                            d <- d + by - (ny + new_size - 1)
                        }
                        if (bz < nz) {
                            d <- d + nz - bz
                        } else if (bz > nz + new_size - 1) {
                            d <- d + bz - (nz + new_size - 1)
                        }

                        if (d <= br) {
                            count <- count + 1
                        }
                    }

                    new_distance <- min_distance_to_origin(nx, ny, nz, new_size)
                  
                    new_entry <- c(-count, new_distance, new_size, nx, ny, nz)
                    
                    
                    heap <- c(heap, list(new_entry))
                    
                    heap_len <- length(heap)
                    
                    if(heap_len >1 ){
                      
                        
                        j<- heap_len
                        
                        while(j > 1){
                            parent <- j %/% 2
                            
                            if(heap[[j]][1] < heap[[parent]][1] || (heap[[j]][1] == heap[[parent]][1] && heap[[j]][2] < heap[[parent]][2])){
                                temp <- heap[[j]]
                                heap[[j]] <- heap[[parent]]
                                heap[[parent]] <- temp
                                j<- parent
                            }else{
                              break
                            }

                        }

                    }
                    
                }
            }
        }
    }

    return(best_distance)
}

main <- function() {
    nanobots <- parse_input("input.txt")
    cat("Part One:", part_one(nanobots), "\n")
    cat("Part Two:", part_two(nanobots), "\n")
}

main()
