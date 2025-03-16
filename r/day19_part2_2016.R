
josephus <- function(n) {
    i <- 1
    while (i * 3 <= n) {
        i <- i * 3
    }
    return(n - i + max(n - 2 * i, 0))
}

main <- function() {
    input <- readLines("input.txt")
    num_elves <- as.integer(input)
    result <- josephus(num_elves)
    cat(result)
}

main()
