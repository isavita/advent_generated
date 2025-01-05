
input <- readLines("input.txt", warn = FALSE)
input <- paste(input, collapse = "")
matches <- gregexpr("mul\\([0-9]{1,3},[0-9]{1,3}\\)", input)[[1]]
if(matches[1] == -1) {
  print(0)
} else {
  matches_str <- regmatches(input, gregexpr("mul\\([0-9]{1,3},[0-9]{1,3}\\)", input))[[1]]
  nums <- gsub("mul\\(|\\)", "", matches_str)
  nums_split <- strsplit(nums, ",")
  nums_matrix <- matrix(as.integer(unlist(nums_split)), ncol = 2, byrow = TRUE)
  print(sum(nums_matrix[,1] * nums_matrix[,2]))
}
