
input <- readLines("input.txt", warn = FALSE)
input <- paste(input, collapse = "")
matches <- gregexpr("(mul\\([0-9]{1,3},[0-9]{1,3}\\))|(do\\(\\))|(don't\\(\\))", input)[[1]]
lengths <- attr(matches, "match.length")
enabled <- TRUE
totalSum <- 0
for (i in seq_along(matches)) {
    start <- matches[i]
    end <- start + lengths[i] - 1
    match <- substr(input, start, end)
    if (grepl("^mul", match)) {
        if (enabled) {
            nums <- as.integer(strsplit(gsub("[^0-9,]", "", match), ",")[[1]])
            totalSum <- totalSum + prod(nums)
        }
    } else if (match == "do()") {
        enabled <- TRUE
    } else if (match == "don't()") {
        enabled <- FALSE
    }
}
cat(totalSum, "\n")
