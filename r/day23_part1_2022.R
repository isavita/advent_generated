
# Function to read the initial state of the Elves from a file
read_initial_state <- function(filename) {
    lines <- readLines(filename)
    elves <- matrix(FALSE, nrow = length(lines), ncol = nchar(lines[1]))
    for (i in seq_along(lines)) {
        for (j in 1:nchar(lines[i])) {
            if (substr(lines[i], j, j) == '#') {
                elves[i, j] <- TRUE
            }
        }
    }
    return(elves)
}

# Function to calculate the proposed moves for each Elf
calculate_proposed_moves <- function(elves, directions_order) {
  
  
    proposals <- list()
    elf_coords <- which(elves, arr.ind = TRUE)
    num_elves <- nrow(elf_coords)

    for (i in 1:num_elves) {
        r <- elf_coords[i, 1]
        c <- elf_coords[i, 2]

        # Check if there are any neighbors
        has_neighbors <- FALSE
        for (dr in -1:1) {
            for (dc in -1:1) {
                if (dr == 0 && dc == 0) next
                nr <- r + dr
                nc <- c + dc
                if (nr >= 1 && nr <= nrow(elves) && nc >= 1 && nc <= ncol(elves) && elves[nr, nc]) {
                    has_neighbors <- TRUE
                    break
                }
            }
            if (has_neighbors) break
        }
        if (!has_neighbors) {
            proposals[[i]] <- c(r, c) # Stay in place
            next
        }

        proposed <- FALSE
        for (dir in directions_order) {
            can_move <- TRUE
            if (dir == "N") {
                for (dc in -1:1) {
                  nr <- r - 1
                  nc <- c + dc
                  if(nr >= 1 && nr <= nrow(elves) && nc >=1 && nc <= ncol(elves) && elves[nr, nc]){
                    can_move <- FALSE
                    break
                  }
                }
                if(can_move){
                  proposals[[i]] <- c(r - 1, c)
                  proposed <- TRUE
                  break
                }
            } else if (dir == "S") {
                for (dc in -1:1) {
                  nr <- r + 1
                  nc <- c + dc
                  if(nr >= 1 && nr <= nrow(elves) && nc >=1 && nc <= ncol(elves) && elves[nr, nc]){
                    can_move <- FALSE
                    break
                  }
                }
                if(can_move){
                  proposals[[i]] <- c(r + 1, c)
                  proposed <- TRUE
                  break
                }
            } else if (dir == "W") {
               for (dr in -1:1) {
                  nr <- r + dr
                  nc <- c - 1
                  if(nr >= 1 && nr <= nrow(elves) && nc >=1 && nc <= ncol(elves) && elves[nr, nc]){
                    can_move <- FALSE
                    break
                  }
                }
                if(can_move){
                  proposals[[i]] <- c(r, c - 1)
                  proposed <- TRUE
                  break
                }
            } else if (dir == "E") {
                for (dr in -1:1) {
                  nr <- r + dr
                  nc <- c + 1
                  if(nr >= 1 && nr <= nrow(elves) && nc >=1 && nc <= ncol(elves) && elves[nr, nc]){
                    can_move <- FALSE
                    break
                  }
                }
                if(can_move){
                  proposals[[i]] <- c(r, c + 1)
                  proposed <- TRUE
                  break
                }
            }
        }
          if(!proposed) {
            proposals[[i]] <- c(r,c)
          }
    }
    return(proposals)
}


# Function to move the Elves based on the proposals
move_elves <- function(elves, proposals) {
  
    proposal_counts <- table(sapply(proposals, function(p) paste(p, collapse = ",")))
    new_elves <- matrix(FALSE, nrow = nrow(elves) + 2, ncol = ncol(elves) + 2) # Add padding
    
    elf_coords <- which(elves, arr.ind = TRUE)

    for(i in 1:length(proposals)){
      prop_str <- paste(proposals[[i]], collapse = ",")
      if(proposal_counts[[prop_str]] == 1){
          # Adjust proposal coordinates for the padding
          new_r <- proposals[[i]][1] + 1
          new_c <- proposals[[i]][2] + 1
          new_elves[new_r, new_c] <- TRUE
      } else{
          # Adjust original elf coordinates for padding
          old_r <- elf_coords[i,1] + 1
          old_c <- elf_coords[i,2] + 1
          new_elves[old_r, old_c] <- TRUE #stay in the same place (adjusted for padding)
      }
    }

    return(new_elves)
}

# Function to calculate the number of empty tiles
calculate_empty_tiles <- function(elves) {
    elf_coords <- which(elves, arr.ind = TRUE)
    min_row <- min(elf_coords[, 1])
    max_row <- max(elf_coords[, 1])
    min_col <- min(elf_coords[, 2])
    max_col <- max(elf_coords[, 2])

    area <- (max_row - min_row + 1) * (max_col - min_col + 1)
    num_elves <- sum(elves)
    return(area - num_elves)
}


# Main function
main <- function() {
    elves <- read_initial_state("input.txt")
    directions_order <- c("N", "S", "W", "E")

    for (round in 1:10) {
        proposals <- calculate_proposed_moves(elves, directions_order)
        elves <- move_elves(elves, proposals)
        
        #Remove empty rows and cols:
        while(sum(elves[1,]) == 0) elves <- elves[-1,,drop=FALSE]
        while(sum(elves[nrow(elves),]) == 0) elves <- elves[-nrow(elves),,drop=FALSE]
        while(sum(elves[,1]) == 0) elves <- elves[,-1,drop=FALSE]
        while(sum(elves[,ncol(elves)]) == 0) elves <- elves[,-ncol(elves),drop=FALSE]

        # Rotate directions
        directions_order <- c(directions_order[-1], directions_order[1])
    }

    empty_tiles <- calculate_empty_tiles(elves)
    cat(empty_tiles, "\n")
}

main()
