
mix <- function(nums) {
    n <- length(nums) - 1
    l <- length(nums)
    for (i in 1:l) {
        oldpos <- nums[[i]][1]
        newpos <- ((oldpos + nums[[i]][2]) %% n + n) %% n
        if (oldpos < newpos) {
            for (j in 1:l) {
                if (nums[[j]][1] > oldpos && nums[[j]][1] <= newpos) {
                  nums[[j]][1] <- nums[[j]][1] - 1
                }
            }
        }
        if (newpos < oldpos) {
            for (j in 1:l) {
                if (nums[[j]][1] >= newpos && nums[[j]][1] < oldpos) {
                  nums[[j]][1] <- nums[[j]][1] + 1
                }
            }
        }
        nums[[i]][1] <- newpos
    }
  nums
}

coords <- function(nums) {
    l <- length(nums)
    zeroPos <- 0
    for (i in 1:l) {
        if (nums[[i]][2] == 0) {
            zeroPos <- nums[[i]][1]
            break
        }
    }
    sum_val <- 0
    for (i in 1:l) {
        if (nums[[i]][1] == (zeroPos + 1000) %% l || nums[[i]][1] == (zeroPos + 2000) %% l || nums[[i]][1] == (zeroPos + 3000) %% l) {
            sum_val <- sum_val + nums[[i]][2]
        }
    }
    sum_val
}

main <- function() {
    lines <- readLines("input.txt")
    nums <- list()
    for (i in 1:length(lines)) {
        nums[[i]] <- c(i -1, as.integer(lines[i]))
    }

    nums <- mix(nums)
    cat(coords(nums), "\n")

}

main()
