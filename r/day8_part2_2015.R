
data <- readLines("input.txt")

# Part One
total_code <- sum(nchar(data))
total_memory <- sum(nchar(gsub('\\\\x[0-9a-f]{2}|\\\\\"|\\\\\\\\', 'X', data)) - 2)
answer_part_one <- total_code - total_memory
print(answer_part_one)

# Part Two
total_encoded <- sum(nchar(gsub('\\\\|\"', '\\\\&', data)) + 2)
answer_part_two <- total_encoded - total_code
print(answer_part_two)
