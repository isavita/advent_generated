
data <- readLines("input.txt")
message_matrix <- t(sapply(strsplit(data, ""), unlist))
corrected_message <- apply(message_matrix, 2, function(x) {
  table_factor <- table(x)
  names(table_factor)[which.max(table_factor)]
})
cat(corrected_message, sep = "")
