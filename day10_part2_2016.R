
input <- readLines("input.txt")

bot_instructions <- list()
bot_values <- list()
output_bins <- list()

for (line in input) {
  if (grepl("value", line)) {
    parts <- strsplit(line, " ")[[1]]
    value <- as.integer(parts[2])
    bot <- as.integer(parts[6])
    
    if (!(bot %in% names(bot_values))) {
      bot_values[[as.character(bot)]] <- c()
    }
    
    bot_values[[as.character(bot)]] <- c(bot_values[[as.character(bot)]], value)
  } else {
    bot <- as.integer(strsplit(line, " ")[[1]][2])
    low_type <- strsplit(line, " ")[[1]][6]
    low_dest <- as.integer(strsplit(line, " ")[[1]][7])
    high_type <- strsplit(line, " ")[[1]][11]
    high_dest <- as.integer(strsplit(line, " ")[[1]][12])
    
    bot_instructions[[as.character(bot)]] <- c(low_type, low_dest, high_type, high_dest)
  }
}

while (length(bot_values) > 0) {
  for (bot in names(bot_values)) {
    if (length(bot_values[[bot]]) == 2) {
      values <- sort(bot_values[[bot]])
      low <- values[1]
      high <- values[2]
      
      if (low == 17 && high == 61) {
        print(bot)
      }
      
      if (bot_instructions[[bot]][1] == "bot") {
        bot_values[[as.character(bot_instructions[[bot]][2])]] <- c(bot_values[[as.character(bot_instructions[[bot]][2])]], low)
      } else {
        output_bins[[as.character(bot_instructions[[bot]][2])]] <- low
      }
      
      if (bot_instructions[[bot]][3] == "bot") {
        bot_values[[as.character(bot_instructions[[bot]][4])]] <- c(bot_values[[as.character(bot_instructions[[bot]][4])]], high)
      } else {
        output_bins[[as.character(bot_instructions[[bot]][4])]] <- high
      }
      
      bot_values[[bot]] <- NULL
    }
  }
}

output0 <- output_bins[["0"]]
output1 <- output_bins[["1"]]
output2 <- output_bins[["2"]]

print(output0 * output1 * output2)
