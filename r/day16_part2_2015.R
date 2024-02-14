
data <- readLines("input.txt")

# Data processing
mfcsam_data <- strsplit(data, ", |: |\\s+")
aunts_data <- matrix(unlist(mfcsam_data), ncol = 2, byrow = TRUE)
aunts_data <- data.frame(aunts_data, stringsAsFactors = FALSE)
names(aunts_data) <- c("attribute", "value")

# Define the MFCSAM readings
mfcsam_readings <- data.frame(
  attribute = c("children", "cats", "samoyeds", "pomeranians", "akitas", "vizslas", "goldfish", "trees", "cars", "perfumes"),
  value = c(3, 7, 2, 3, 0, 0, 5, 3, 2, 1),
  stringsAsFactors = FALSE
)

# Check which Aunt Sue matches the MFCSAM readings
matches <- rep(TRUE, nrow(aunts_data))
for (i in 1:nrow(mfcsam_readings)) {
  attribute <- mfcsam_readings[i, "attribute"]
  value <- as.numeric(mfcsam_readings[i, "value"])
  
  if (attribute %in% names(aunts_data)) {
    matches <- matches & aunts_data[, attribute] == value
  }
}

# Print the number of the Aunt Sue that matches the MFCSAM readings
print(which(matches))
